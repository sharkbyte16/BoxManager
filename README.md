What is BoxManager?
===================
BoxManager is a system configuration manager for the 86Box PC emulator on Linux.

![alt text](https://github.com/sharkbyte16/BoxManager/blob/main/images/Screenshot%20BoxManager.png?raw=true)

Why?
----
The short answer: To scratch my itch. 
The slightly longer answer: PCem is great emulator for running retro virtual PCs on Linux. It has a nice no frills GUI to configure and manage the virtual machines. However, at the moment it is in maintenance mode without apparent feature development. 86Box on the other hand is actively developed, has more features and is more accurate. And while the virtual machines settings configurator is good, it lacks a GUI for managing the virtual machines. Hence this program to manage virtual machines. It started with a copy the of the virtual machine management part of PCem to 86Box, now it has a fresh interface and some additional features.

Features
--------
- Main functions: Adding, renaming, copying, editing, documenting and running system configurations of 86Box.
- Each  configuration is stored in its own folder in the config folder: ~/.config/BoxManager/VMs.
- Each 86Box system has it's own nvr folder for non-volatile memory files.
- Editing system configurations is performed by 86Box itself (thus always up to date).
- Automatic backup of system configurations including the hard disk images (with user adjustable max hard disk image size limit).
- Run options: start in fullscreen mode, no confirmation on quit and a free manual options text field.
- Notes on the installed OS and apps can be added (stored in os.txt and apps.txt in the configuration folder)
- Simulation of a Tulip Dual Graphics Adapter (DGA): if DGA enabled in the settings and if both Hercules and IBM CGA settings are present in the VM config file and one of these is the current graphics card, a selector is shown for easy switching between the two graphic modes.

Build
-----
BoxManager is written in Lazarus 3.6+ on Linux. To build, just open the .lpr file in Lazarus.
The binary folder contains a prebuild executable (build on Linux Mint LMDE 6 / Debian 12) you can try running without building it yourself.

  


