What is BoxManager?
===================
BoxManager is a system configuration manager for the 86Box PC emulator on Linux (and Windows). It implements a clone of the PCem interface for 86Box.

![alt text](https://github.com/sharkbyte16/BoxManager/blob/main/images/Screenshot%20BoxManager.png?raw=true)

Why?
----
The short answer: To scratch my itch. 
The slightly longer answer: PCem is great emulator for running retro virtual PCs on Linux. It has a nice no frills GUI to configure and manage the virtual machines. However, at the moment it is in maintenance mode without apparent feature development. 86Box on the other hand is actively developed, has more features and is more accurate. And while the virtual machines settings configurator is good, it lacks a GUI for managing the virtual machines. Hence this program to copy the virtual machine management part of PCem to 86Box.

Features
--------
- Main functions: Adding, renaming, copying, editing and running system configurations in 86Box.
- Each  configuration is stored in its own folder: ~/.config/BoxManager/VM on Linux, %localappdata%\BoxManager\VM on Windows.
- Each system has it's own nvr folder for non-volatile memory files.
- Editing system configurations is performed by 86Box itself (thus always up tot date).
- Run options: start in fullscreen mode and no confirmation on quit (although 86Box seems to ignore --noconfirm regularly).


Code
----
BoxManager is written in Lazarus (3.2) on Ubuntu 21.10, and at intervals tested on Windows 10 22H2.
To build and run, just open the .lpr file in Lazarus and hit run :). The Win32 and Win64 foulder contain prebuild windows binaries.

  


