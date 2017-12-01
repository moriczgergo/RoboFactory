@ECHO OFF
rgbasm -omain.obj main.asm
xlink -mmain.map -nmain.sym LINK
rgbfix -v robofactory.gbc
bgb robofactory.gbc