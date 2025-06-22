What is BoxManager?
===================
BoxManager is a system configuration manager that provides a GUI wrapper for the 86Box PC emulator on Linux. It focuses on providing a user-friendly interface for managing virtual machine configurations, automating backups, and organizing VM-related files.

![alt text](https://github.com/sharkbyte16/BoxManager/blob/main/images/Screenshot%20BoxManager.png?raw=true)

Features
--------

- VM Lifecycle Management: Create, copy, rename, delete, and launch VMs through a unified interface
- Automatic Backups: Configurable backup system with size limits for hard disk images
- Documentation System: Store OS and application notes per VM
- Tulip DGA Simulation: Dual Graphics Adapter simulation for switching between Hercules and CGA modes
- Flexible Launch Options: Fullscreen mode, no-quit confirmation, and custom parameters

Build
-----
BoxManager is developed using the Lazarus IDE 3.6+ and FreePascal compiler) on Linux. To build BoxManager,  open the .lpr file in Lazarus and compile directly. The binary folder contains a prebuild executable (build on Linux Mint LMDE 6 / Debian 12) you may try to run without building form source.
