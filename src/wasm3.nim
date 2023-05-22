import wasm3/[wasm3c]
# This stuff here is likely to get moved to another module eventually
import std/[macros, genasts, typetraits, enumerate, tables, strformat]
import micros

export wasm3c

const
  wasm3AllocName {.strdefine.} = "alloc"
  wasm3DeallocName {.strdefine.} = "dealloc"

type
  WasmError* = object of CatchableError

  WasmEnv* = ref object # ref counting is good for the soul
    env: PEnv
    runtime: PRuntime
    modules: seq[PModule]
    wasmData: seq[seq[uint8]] # have to keep data alive
    allocFunc, deallocFunc: PFunction

  WasmHostProc* = object
    module, name, typ: string
    prc: WasmProc

import wasm3/wasmconversions
export wasmconversions


proc wasmValidTuple*(t: typedesc[tuple]): bool =
  result = true
  for field in fields t():
    when field isnot WasmTypes:
      return false

type
  WasmTuple* = concept type WT
    WT is tuple
    wasmValidTuple(WT)

  AllowedWasmType* = WasmTypes or void or WasmTuple

var currentRuntime: PRuntime = nil

proc printStackTrace*(runtime: PRuntime) =
  let backtrace = m3_GetBacktrace(runtime)
  if backtrace.isNil:
    return
  echo "Wasm Stacktrace:"
  var temp = backtrace.frames
  while not temp.isNil:
    let module = m3_GetFunctionModule(temp.function)
    let name = m3_GetFunctionName(temp.function)
    echo m3_GetModuleName(module), ".", name
    temp = temp.next

proc m3PrintStackTrace*(runtime: PRuntime) {.exportc.} =
  let runtime = if runtime.isNil: currentRuntime else: runtime
  if runtime.isNil:
    return
  printStackTrace(runtime)

proc checkWasmRes*(res: Result) {.inline.} =
  if res != nil:
    if not currentRuntime.isNil:
      currentRuntime.printStackTrace()
    raise newException(WasmError, $res)

proc suppressLookupFailure*(res: Result) {.inline.} =
  discard

proc `=destroy`(we: var typeof(WasmEnv()[])) =
  m3FreeRuntime(we.runtime)
  m3FreeEnvironment(we.env)
  `=destroy`(we.wasmData)

proc findFunction*(wasmEnv: WasmEnv, name: string, args, results: openarray[ValueKind]): PFunction

proc getUserData*(wasmEnv: WasmEnv): pointer =
  return m3_GetUserData(wasmEnv.runtime)

proc toByteArray(a: sink string): seq[uint8] =
  result.setLen(a.len)
  for i in 0..high(a):
    result[i] = a[i].uint8

proc loadWasmEnv*(
  wasmData: openArray[uint8],
  stackSize: uint32 = high(uint16),
  hostProcs: openarray[WasmHostProc] = [],
  loadAlloc = false,
  allocName = wasm3AllocName,
  deallocName = wasm3DeallocName,
  userdata: pointer,
  ): WasmEnv =

  new result
  result.wasmData = @[@wasmData]
  result.env = m3_NewEnvironment()
  result.runtime = result.env.m3_NewRuntime(stackSize, userdata)

  result.modules.add default(PModule)
  checkWasmRes m3_ParseModule(result.env, result.modules[0].addr, result.wasmData[0][0].addr, uint32 result.wasmData[0].len)
  try:
    checkWasmRes m3_LoadModule(result.runtime, result.modules[0])
  except WasmError:
    m3FreeModule(result.modules[0])
    raise

  when defined wasm3HasWasi: # Maybe an if statement?
    checkWasmRes m3LinkWasi(result.modules[0])
  for hostProc in hostProcs:
    suppressLookupFailure m3LinkRawFunction(result.modules[0], cstring hostProc.module, cstring hostProc.name, cstring hostProc.typ, hostProc.prc)
  checkWasmRes m3_CompileModule(result.modules[0])

  if loadAlloc:
    result.allocFunc = result.findFunction(allocName, [I32], [I32])
    result.deallocFunc = result.findFunction(deallocName, [I32], [])

proc loadWasmEnv*(
  wasmData: sink string,
  stackSize: uint32 = high(uint16),
  hostProcs: openarray[WasmHostProc] = [],
  loadAlloc = false,
  allocName = wasm3AllocName,
  deallocName = wasm3DeallocName,
  userdata: pointer,
  ): WasmEnv =

  new result
  result.wasmData = @[wasmData.toByteArray]
  result.env = m3_NewEnvironment()
  result.runtime = result.env.m3_NewRuntime(stackSize, userdata)

  result.modules.add default(PModule)
  checkWasmRes m3_ParseModule(result.env, result.modules[0].addr, result.wasmData[0][0].addr, uint32 result.wasmData[0].len)
  try:
    checkWasmRes m3_LoadModule(result.runtime, result.modules[0])
  except WasmError:
    m3FreeModule(result.modules[0])
    raise

  when defined wasm3HasWasi: # Maybe an if statement?
    checkWasmRes m3LinkWasi(result.modules[0])
  for hostProc in hostProcs:
    suppressLookupFailure m3LinkRawFunction(result.modules[0], cstring hostProc.module, cstring hostProc.name, cstring hostProc.typ, hostProc.prc)
  checkWasmRes m3_CompileModule(result.modules[0])

  if loadAlloc:
    result.allocFunc = result.findFunction(allocName, [I32], [I32])
    result.deallocFunc = result.findFunction(deallocName, [I32], [])

proc loadWasmEnv*(
  wasmData: sink openarray[string],
  stackSize: uint32 = high(uint16),
  hostProcs: openarray[WasmHostProc] = [],
  loadAlloc = false,
  allocName = wasm3AllocName,
  deallocName = wasm3DeallocName,
  userdata: pointer,
  ): WasmEnv =

  new result
  result.env = m3_NewEnvironment()
  result.runtime = result.env.m3_NewRuntime(stackSize, userdata)
  result.wasmData.setLen(wasmData.len)
  result.modules.setLen(wasmData.len)
  for i, data in enumerate result.wasmData.mitems:
    data = wasmData[i].toByteArray

    checkWasmRes m3_ParseModule(result.env, result.modules[i].addr, cast[ptr uint8](result.wasmData[i][0].addr), uint32 result.wasmData[i].len)
    try:
      checkWasmRes m3_LoadModule(result.runtime, result.modules[i])
    except WasmError:
      m3FreeModule(result.modules[i])
      raise

  when defined wasm3HasWasi: # Maybe an if statement?
    for module in result.modules:
      checkWasmRes m3LinkWasi(module)

  for hostProc in hostProcs:
    for module in result.modules:
      suppressLookupFailure m3LinkRawFunction(module, cstring hostProc.module, cstring hostProc.name, cstring hostProc.typ, hostProc.prc)

  for module in result.modules:
    checkWasmRes m3_CompileModule(module)

  if loadAlloc:
    result.allocFunc = result.findFunction(allocName, [I32], [I32])
    result.deallocFunc = result.findFunction(deallocName, [I32], [])

proc ptrArrayTo*(t: var WasmTypes): array[1, pointer] = [pointer(addr t)]

proc ptrArrayTo*(t: var WasmTuple): auto =
 result = default(array[tupleLen(t), pointer])
 for i, x in enumerate t.fields:
   result[i] = pointer(x.addr)

template getResult*[T: WasmTuple or WasmTypes](theFunc: PFunction): untyped =
  when T is void:
    discard
  else:
    var
      res: T
      ptrArray = res.ptrArrayTo
    checkWasmRes m3_GetResults(theFunc, uint32 ptrArray.len, cast[ptr pointer](ptrArray.addr))
    res

macro call*(theFunc: PFunction, returnType: typedesc[WasmTuple or WasmTypes or void],  args: varargs[typed]): untyped =
  ## Calls
  result = newStmtList()
  let arrVals = nnkBracket.newTree()
  for arg in args:
    let argName = genSym(nskVar)
    result.add:
      genast(argName, arg):
        var argName = arg
    arrVals.add:
      genAst(argName):
        pointer(addr argName)

  if args.len > 0:
    result.add:
      genast(returnType, theFunc, arrVals, callProc = bindsym"m3_Call", m3_GetFunctionModule = bindsym"m3_GetFunctionModule", m3_GetModuleRuntime = bindsym"m3_GetModuleRuntime"):
        var arrVal = arrVals
        currentRuntime = m3_GetModuleRuntime(m3_GetFunctionModule(theFunc))
        checkWasmRes callProc(theFunc, uint32 len arrVal, cast[ptr pointer](arrVal.addr))
        getResult[returnType](theFunc)
  else:
    result.add:
        genast(returnType, theFunc, callProc = bindsym"m3_Call", m3_GetFunctionModule = bindsym"m3_GetFunctionModule", m3_GetModuleRuntime = bindsym"m3_GetModuleRuntime"):
          checkWasmRes callProc(theFunc, 0, nil)
          currentRuntime = m3_GetModuleRuntime(m3_GetFunctionModule(theFunc))
          getResult[returnType](theFunc)

macro callHost*(p: proc, stackPointer: var uint64, mem: pointer): untyped =
  ## This takes a proc, stackPointer and mem.
  ## It emits `fromWasm` for each argument, and `ptr ReturnType` for the return value.
  ## It then calls the proc with args and sets the return value.
  let
    typ = p.getType()
    retT = typ[1]
    hasReturnType = not typ[1].getType.sameType(getType(void))
    call = newCall(p)
  for typ in typ[2..^1]:
    call.add:
      genast(stackPointer, mem, typ = typ.getTypeInst()):
        let arg = block:
          var val = default(typeof(typ))
          val.fromWasm(stackPointer, mem)
          val
        arg
  result =
    if hasReturnType:
      genast(retT, mem, call, stackPointer):
        let retType = block:
          var val: ptr retT
          val.fromWasm(stackPointer, mem)
          val
        retType[] = call
    else:
      call

proc wasmHostProc*(module, name, typ: string, prc: WasmProc): WasmHostProc =
  WasmHostProc(module: module, name: name, typ: typ, prc: prc)

template toWasmHostProc*(p: static proc, modul, nam, ty: string): WasmHostProc =
  WasmHostProc(
    module: modul,
    name: nam,
    typ: ty,
    prc: proc (runtime: PRuntime; ctx: PImportContext; sp: ptr uint64; mem: pointer): pointer {.cdecl.} =
      var sp = stackPtrToUint(sp)
      callHost(p, sp, mem)
    )

macro toWasmHostProcTemplate*(procedureTemplate: typed, modul, nam, ty: string): WasmHostProc =
  result = genAst(procedureTemplate, modul, nam, ty):
    WasmHostProc(
      module: modul,
      name: nam,
      typ: ty,
      prc: proc (runtime: PRuntime; ctx: PImportContext; sp: ptr uint64; mem: pointer): pointer {.cdecl.} =
        currentRuntime = runtime
        procedureTemplate(runtime, procedure)
        var sp = stackPtrToUint(sp)
        callHost(procedure, sp, mem)
      )

proc isType*(fnc: PFunction, args, results: openArray[ValueKind]): bool =
  # Returns whether a wasm module's function matches the type signature supplied.
  result = true
  if m3_GetRetCount(fnc) != uint32(results.len) or m3_GetArgCount(fnc) != uint32(args.len):
    return false
  for i, arg in args:
    if arg != m3_GetArgType(fnc, uint32 i):
      return false
  for i, res in results:
    if res != m3_GetRetType(fnc, uint32 i):
      return false

proc findFunction*(wasmEnv: WasmEnv, name: string): PFunction =
  checkWasmRes m3FindFunction(result.addr, wasmEnv.runtime, cstring name)

proc findFunction*(wasmEnv: WasmEnv, name: string, args, results: openarray[ValueKind]): PFunction =
  result = wasmEnv.findFunction(name)
  if not result.isType(args, results):
    {.warning: "Insert rendered proc here".}
    raise newException(WasmError, "Function is not the type requested.")

proc findGlobal*(wasmEnv: WasmEnv, name: string): PGlobal =
  result = m3FindGlobal(wasmEnv.modules[0], cstring name)
  if result.isNil:
    raise newException(WasmError, "Global named '" & name & "' is not found.")

proc getGlobal*(global: PGlobal): WasmVal =
  checkWasmRes m3GetGlobal(global, result.addr)

proc getGlobal*(wasmEnv: WasmEnv, name: string): WasmVal =
  wasmEnv.findGlobal(name).getGlobal()

proc getFromMem*(wasmEnv: WasmEnv, T: typedesc, pos: uint32, offset: uint64 = 0): T =
  var sizeOfMem: uint32
  let thePtr = m3GetMemory(wasmEnv.runtime, addr sizeOfMem, 0)
  if pos + uint32(sizeof(T)) + uint32(offset) > sizeOfMem:
    raise newException(WasmError, "Attempted to read outside of memory bounds")
  copyMem(result.addr, cast[pointer](cast[uint64](thePtr) + uint64(pos) + offset), sizeof(T))

proc toRawPtr*(wasmEnv: WasmEnv, pos: WasmPtr, T: typedesc): ptr T =
  var sizeOfMem: uint32
  let thePtr = m3GetMemory(wasmEnv.runtime, addr sizeOfMem, 0)
  if pos.uint32 >= sizeOfMem:
    raise newException(WasmError, "Attempted to read outside of memory bounds")
  return cast[ptr T](cast[uint64](thePtr) + uint64(pos))

proc toRawPtr*(wasmEnv: WasmEnv, pos: WasmPtr): pointer =
  var sizeOfMem: uint32
  let thePtr = m3GetMemory(wasmEnv.runtime, addr sizeOfMem, 0)
  if pos.uint32 >= sizeOfMem:
    raise newException(WasmError, "Attempted to read outside of memory bounds")
  return cast[pointer](cast[uint64](thePtr) + uint64(pos))

proc find*[T](wasmEnv: WasmEnv, pos: WasmPtr, value: T): WasmPtr =
  var sizeOfMem: uint32
  let thePtr = m3GetMemory(wasmEnv.runtime, addr sizeOfMem, 0)
  result = pos
  var pos = pos.uint32
  while pos + uint32(sizeof(T)) - 1 < sizeOfMem:
    if cast[ptr T](cast[uint64](thePtr) + uint64(pos))[] == value:
      return pos.WasmPtr
    pos += uint32(sizeof(T))
  return 0.WasmPtr

proc getString*(wasmEnv: WasmEnv, pos: WasmPtr): cstring =
  let posEnd = wasm3.find(wasmEnv, pos, 0.uint8)
  let len = posEnd.uint32 - pos.uint32

  return cast[cstring](wasmEnv.toRawPtr(pos))

proc setMem*[T](wasmEnv: WasmEnv, val: T, pos: uint32, offset: uint64 = 0) =
  mixin wasmSize
  var sizeOfMem: uint32
  let thePtr = m3GetMemory(wasmEnv.runtime, addr sizeOfMem, 0)
  if pos + uint32(sizeof(T)) + uint32(offset) > sizeOfMem:
    raise newException(WasmError, "Attempted to write outside of memory bounds")
  copyMem(cast[pointer](cast[uint64](thePtr) + uint64(pos) + offset), val.unsafeAddr, sizeof typeof(val))

proc setMem*[T](wasmEnv: WasmEnv, val: T, pos: WasmPtr, offset: uint64 = 0) =
  setMem(wasmEnv, val, uint32(pos), offset)

proc copyMem*(wasmEnv: WasmEnv, pos: uint32, p: pointer, len: int, offset = 0u32) =
  var sizeOfMem: uint32
  let thePtr = m3GetMemory(wasmEnv.runtime, addr sizeOfMem, 0)
  if pos + uint32(len) > sizeOfMem:
    raise newException(WasmError, "Attempted to write outside of memory bounds")
  copyMem(cast[pointer](cast[uint64](thePtr) + pos.uint64 + offset.uint64), p, len)

proc copyMem*(wasmEnv: WasmEnv, pos: WasmPtr, p: pointer, len: int, offset = 0u32) =
  copyMem(wasmEnv, uint32 pos, p, len)

proc copyTo*[T: WasmAllocatable](wasmEnv: WasmEnv, data: T): WasmPtr =
  mixin wasmCopyTo, wasmSize
  let size = data.wasmSize()
  result = WasmPtr(wasmEnv.allocFunc.call(uint32, size))
  var memSize: uint32
  let dest = wasmEnv.env.m3GetMemory(memSize.addr, uint32 result)
  data.wasmCopyTo(dest)


proc alloc*(env: WasmEnv, size: uint32): WasmPtr =
  if env.allocFunc.isNil:
    raise newException(WasmError, fmt"Environment's '{wasm3AllocName}' procedure is 'nil'")
  env.allocFunc.call(WasmPtr, size)

proc dealloc*(env: WasmEnv, thePtr: WasmPtr) =
  if env.deallocFunc.isNil:
    raise newException(WasmError, fmt"Environment's '{wasm3DeallocName}' procedure is 'nil'")
  env.deallocFunc.call(void, thePtr)

proc alloc*[T: WasmAllocatable](env: WasmEnv, allocatable: T): WasmPtr =
  mixin wasmAlloc
  mixin wasmSize
  const size = wasmSize(T)
  result = env.alloc(size)
  wasmAlloc(allocatable, env, result)


