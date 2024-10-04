What is BoxManager?
===================
BoxManager2 is a system configuration manager for the 86Box and PCem PC emulators on Linux. It implements a clone of the PCem interface for use on both emulators.

![alt text](https://github.com/sharkbyte16/BoxManager/blob/main/images/Screenshot%20BoxManager.png?raw=true)

Why?
----
The short answer: To scratch my itch. 
The slightly longer answer: PCem is great emulator for running retro virtual PCs on Linux. It has a nice no frills GUI to configure and manage the virtual machines. However, at the moment it is in maintenance mode without apparent feature development. 86Box on the other hand is actively developed, has more features and is more accurate. And while the virtual machines settings configurator is good, it lacks a GUI for managing the virtual machines. Hence this program to copy the virtual machine management part of PCem to 86Box and for convenience combine config management for both emulators in one app.

Version 2
---------
The first version was my first try to make an app. It was thrown together without design. This version is a complete rewrite with more structured VM management code separated from the GUI code. This also allowed to add managing of PCem VMs and allows to easily change the GUI in the future for version 3.11 ;-)

Features
--------
- Main functions: Adding, renaming, copying, editing and running system configurations in 86Box and PCem.
- Each  configuration is stored in its own folder in the config folder: ~/.config/BoxManager2/VM (on Windows %localappdata%\BoxManager\VM).
- Each 86Box system has it's own nvr folder for non-volatile memory files. For PCem, the nvr files are stored in the central PCem nvr folder.
- Editing system configurations is performed by 86Box itself (thus always up tot date). Likewise for PCem, however only within a running VM due to a command line limitation of PCem.
- Automatic backup of system configurations including the hard disk images (with user adjustable max hard disk image size limit).
- Run options: start in fullscreen mode (both) and no confirmation on quit (86Box only).

Code
----
BoxManager2 is written in Lazarus 3.4 on Linux. To build and run, just open the .lpr file in Lazarus and hit run :). It should also compile and run on Windows, however YMMV.

  


