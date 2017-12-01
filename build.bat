@ECHO OFF
rgbasm -omain.obj main.asm
xlink -mmain.map -nmain.sym LINK
rgbfix -v main.gbc
bgb robofactory.gbc