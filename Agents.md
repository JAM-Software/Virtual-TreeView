# AI Coding Assistant Guide for ShellBrowser

This document provides essential information for AI coding assistants working with the Virtual TreeView Components codebase.

## Quick Reference

- **Project Type**: Open Soruice Delphi and C++ Builder component library
- **Primary Language**: Object Pascal (Delphi)
- **Main Documentation**: See [`README.md`](Readme.md) for technology stack, supported versions of RAD Studio / Delphi and project overview

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

### All relevant code

Always build all relevant code after making chnages to see if everything compiles using:

```cmd
RsVars.bat
MsBuild.exe VirtualTreesDevelopment.groupproj
```
