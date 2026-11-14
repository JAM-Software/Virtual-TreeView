# AI Coding Assistant Guide for ShellBrowser

This document provides essential information for AI coding assistants working with the Virtual TreeView Components codebase.

## Quick Reference

- **Project Type**: Open Soruice Delphi and C++ Builder component library
- **Primary Language**: Object Pascal (Delphi)
- **Main Documentation**: See [`README.md`](Readme.md) for technology stack, supported versions of RAD Studio / Delphi and project overview

## Language

Everything you write to this repository or its GitHub project must be in English: code, identifiers, comments, commit messages, pull requests, issue comments and documentation. This also applies when you talk to the user in another language.

Important Directories

### Source Code

- [`Source/`](Source/) - **ALL runtime Delphi source code** (.pas files)
  - Pure runtime code only - no designtime dependencies
  - Compiled units (.dcu) are generated during build
- [`Design/`](Design/) - **Designtime-only code** for RAD Studio IDE integration
  - Property editors
  - Component registration
  - Design-time helpers
- [`Packages/`](Packages/) - **Delphi and C++ Builder packages** for all supported Delphi versions
  - [`Packages/RAD Studio 10.4+/`](Packages/RAD Studio 10.4+/) - Delphi and C++ Builder packages for all newer versions of RAD Studio, version 10.4 and later.

### Testing

- [`Test/`](Test/) - **Unit-test code** for the DUnitX framework.

### Sample Projects

- [`Demos/`](Demos/) - **sample porjects** showing how to use the compoents.

## How to build

### Unit Tests

See <Readme.md#Running-Tests>

Use the console build of the tests (`Tests\Tests.exe`). Always pass `--exitbehavior:Continue`, otherwise the program waits for the Enter key when it is done. Use `--run:` to run a single test, a whole fixture or a comma-separated list of them:

```cmd
cd Tests
Tests.exe --exitbehavior:Continue --run:VTCellSelectionTests.TCellSelectionTests.TestCopyHTML1
Tests.exe --exitbehavior:Continue --run:VTCellSelectionTests.TCellSelectionTests
```

### All relevant code

Always build all relevant code after making chnages to see if everything compiles using:

```cmd
RsVars.bat
MsBuild.exe VirtualTreesDevelopment.groupproj
```

## Bug fixing

When fixing an issue from the issue thracker, always add a comment that explains the change and include the issue number in the comment.
