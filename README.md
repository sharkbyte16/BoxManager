What is BoxManager?
===================
BoxManager is a system configuration manager for the 86Box PC emulator on Linux. It implements a clone of the PCem interface.

![alt text](https://github.com/sharkbyte16/BoxManager/blob/main/images/Screenshot%20BoxManager.png?raw=true)

Why?
----
The short answer: To scratch my itch. 
The slightly longer answer: PCem is great emulator for running retro virtual PCs on Linux. It has a nice no frills GUI to configure and manage the virtual machines. However, at the moment it is in maintenance mode without apparent feature development. 86Box on the other hand is actively developed, has more features and is more accurate. And while the virtual machines settings configurator is good, it lacks a GUI for managing the virtual machines. Hence this program to copy the virtual machine management part of PCem to 86Box.

Version 2
----------
The first version was my first try to make a Linux application. It was thrown together without design. Version 2 is a complete rewrite with more structured VM management code separated from the GUI code. This also allowed to easily change the GUI in the future.

Features
--------
- Main functions: Adding, renaming, copying, editing, documenting and running system configurations of 86Box.
- Each  configuration is stored in its own folder in the config folder: ~/.config/BoxManager/VM.
- Each 86Box system has it's own nvr folder for non-volatile memory files.
- Editing system configurations is performed by 86Box itself (thus always up tot date).
- Automatic backup of system configurations including the hard disk images (with user adjustable max hard disk image size limit).
- Run options: start in fullscreen mode (both) and no confirmation on quit.
- Notes on the installed OS and apps can be added (stored in os.txt and apps.txt in configuration folder)

Build
-----
BoxManager is written in Lazarus 3.6 on Linux. To build and run, just open the .lpr file in Lazarus and hit run :).

  


