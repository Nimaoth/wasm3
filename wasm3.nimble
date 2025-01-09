# Package

version       = "0.1.17"
author        = "jason"
description   = "A new awesome nimble package"
license       = "MIT"
srcDir        = "src"

skipDirs = @["platforms", "docs", "test"]
skipFiles = @["txt", "py", "zig"]

# Dependencies

requires "nim >= 1.6.6"
requires "https://github.com/beef331/micros/"

import std/[strutils, os]

task buildWasmSources, "Builds all wasmsources and moves them to 'tests'":
  for file in "wasmsources".listFiles:
    if file.endsWith".nim":
      selfExec("c " & file)

task postProcessM3ConfigPlatforms, "":
  let textToAdd = """
#if defined(M3_DISABLE_VECTORCALL)
#  undef vectorcall
#  define vectorcall
#endif"""

  let file = "src/wasm3/wasm3c/source/m3_config_platforms.h"
  var f = readFile(file)
  if f.find(textToAdd) == -1:
    f.add "\n" & textToAdd
    writeFile(file, f)

task setup, "":
  postProcessM3ConfigPlatformsTask()

before install:
  setupTask()
