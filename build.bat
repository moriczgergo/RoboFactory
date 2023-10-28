@ECHO OFF
rgbasm -o main.obj main.asm
rgblink -o robofactory.gbc -m main.map main.obj
rgbfix -p 0 -v robofactory.gbc
bgb robofactory.gbc