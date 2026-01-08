# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SP-Forth (SPF) is a Forth programming language compiler that produces optimized native x86 code. It runs on Windows and Linux (POSIX).

**Key characteristics:**
- Subroutine-threaded code model (EAX=ToS, EBP=data stack, ESP=return stack, EDI=USER variables)
- Built-in peephole optimizer with inlining support
- Module system with EXPORT/DEFINITIONS
- Case-sensitive by default
- x87 hardware floating point support

## Build Commands

### Linux
```bash
cd src/
make PLATFORM=posix build    # or just: make
# Produces: ../spf4
```

### Windows
```bash
cd src/
compile.bat
# Produces: ../spf4.exe
```

### Build Options
Edit `src/compile.ini` (git-ignored) to override defaults in `spf_compileoptions.f`:
```forth
TRUE TO SMALLEST-SPF         # Minimal build
FALSE TO BUILD-OPTIMIZER     # Skip optimizer
FALSE TO CREATE-XML-HELP     # Skip help generation
```

## Architecture

### Source Organization
- `src/` - Core system (GPL). Main build file is `src/spf.f`
- `lib/` - Standard libraries (LGPL): `ext/`, `include/`, `asm/`
- `devel/` - Contributed code organized by author prefix (`~ac/`, `~pinka/`, etc.)
- Platform-specific: `src/posix/` and `src/win/`

### Compilation Pipeline (order in src/spf.f)
1. Kernel primitives (`spf_defkern.f`, `spf_forthproc.f`)
2. Floating point (`spf_floatkern.f`)
3. I/O and platform APIs
4. Module system (`spf_module.f`)
5. Parser/lexer (`compiler/spf_parser.f`)
6. Optimizer (`macroopt.f`) or minimal (`noopt.f`)
7. Code generation (`compiler/spf_compile.f`)
8. Platform features (memory, multitasking, DLL/SO loading)
9. Save/output (`posix/save.f` or `win/spf_pe_save.f`)

### Key Files
- `src/spf.f` - Main build orchestration (~330 lines)
- `src/macroopt.f` - Optimizer core (~150KB)
- `src/spf_defkern.f` - Assembly kernel primitives (~600 lines)
- `src/spf_forthproc.f` - Forth interpreter core (~1000 lines)
- `src/tc_spf.F` - Target compiler interface

### Platform Conditionals
```forth
TARGET-POSIX [IF]
  \ Linux/POSIX code
[ELSE]
  \ Windows code
[THEN]
```

## Forth Conventions

### Word Naming
- `S-` prefix: Takes string (addr u) from stack
- `-ED` suffix: Takes arguments from stack (e.g., `INCLUDED`, `REQUIRED`)
- `?` prefix: Returns boolean flag
- `'` prefix: Gets execution token

### Module Structure
```forth
MODULE: my-lib
\ private code
EXPORT
  \ public interface
DEFINITIONS
\ more private code
;MODULE
```

### Dependencies
```forth
REQUIRE CreateSocket ~ac/lib/win/winsock/sockets.f
REQUIRE ForEach-Word ~pinka/lib/words.f
```

## Debugging

```forth
REQUIRE SEE lib/ext/disasm.f
SEE my-word                  \ View compiled assembly

STARTLOG / ENDLOG            \ Log output to spf.log

DIS-OPT                      \ Disable optimizer
SET-OPT                      \ Enable optimizer
```

## Git Conventions

Commit message format: `[TAG] [scope] -- [description]`
- **UPD** - Update/improvement
- **FIX** - Bug fix
- **FAC** - Refactoring
- **ADD** - New feature

Examples:
```
UPD lib spf-asm -- better `+TO`
FAC src tc -- eliminate code duplication
```
