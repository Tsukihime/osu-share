g++ -O3 -Wall -c -oosu-hook.o osu-hook.cpp
g++ -L%CD% -shared -o..\bin\osu-hook.dll osu-hook.o -lMinHook.x86 -static-libgcc -static-libstdc++