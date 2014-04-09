#include <windows.h>
#include "MinHook.h"

typedef HANDLE (WINAPI *CREATEFILEW)(LPCWSTR, DWORD, DWORD,
		LPSECURITY_ATTRIBUTES, DWORD, DWORD, HANDLE);

// Pointer for calling original CreateFileW.
CREATEFILEW fpCreateFileW = NULL;
void* CreateFileWptr = NULL;

typedef BOOL (WINAPI *SHELLEXECUTEEXW)(LPSHELLEXECUTEINFOW);

// Pointer for calling original ShellExecuteExW.
SHELLEXECUTEEXW fpShellExecuteExW = NULL;
void* ShellExecuteExWptr = NULL;

BOOL disabled = TRUE;
HANDLE mutex = 0;
HANDLE fileMap = 0;
void* mapmemory = NULL;

const int filemapsize = 0x1000;

using namespace std;
#include <sstream>

HANDLE WINAPI DetourCreateFileW(LPCWSTR lpFileName, DWORD dwDesiredAccess,
		DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes,
		DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes,
		HANDLE hTemplateFile) {

	const wchar_t * ext = wcsrchr(lpFileName, '.');

	if (ext != NULL) {
		if (wcscmp(ext, L".mp3") == 0 || (wcscmp(ext, L".ogg") == 0)) {

			int len = (wcslen(lpFileName) + 1) * sizeof(wchar_t);
			if (len > filemapsize) {
				len = filemapsize;
			}

			if ((mutex != 0) && (mapmemory != NULL)) {
				int waitstat = WaitForSingleObject(mutex, 1);
				if ((waitstat == WAIT_OBJECT_0) || (waitstat
						== WAIT_ABANDONED_0)) {
					memcpy(mapmemory, lpFileName, len);
					ReleaseMutex(mutex);
				}
			}
		}
	}

	return fpCreateFileW(lpFileName, dwDesiredAccess, dwShareMode,
			lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes,
			hTemplateFile);
}

BOOL WINAPI DetourShellExecuteExW(LPSHELLEXECUTEINFOW pExecInfo) {

	bool ctrlDown = (GetKeyState(VK_LCONTROL) & 0x8000) || (GetKeyState(
			VK_RCONTROL) & 0x8000);

	bool changed = false;
	wchar_t new_str_buff[MAX_PATH];
	wchar_t* new_str = &new_str_buff[0];
	LPCWSTR pfile = pExecInfo->lpFile;
	const wchar_t* pattern = L"http://osu.ppy.sh/b/";

	if (ctrlDown) {
		const wchar_t * pNum = wcsstr(pExecInfo->lpFile, pattern);
		if (pNum != NULL) {
			pNum += wcslen(pattern);
			memset(new_str, 0, MAX_PATH * sizeof(wchar_t));
			wcscpy(new_str, L"http://bloodcat.com/osu/?q=");
			wcscat(new_str, pNum);
			pExecInfo->lpFile = new_str;
			changed = true;
		}
	}

	if (changed) {
		BOOL res = fpShellExecuteExW(pExecInfo);
		pExecInfo->lpFile = pfile;
		return res;
	} else {
		return fpShellExecuteExW(pExecInfo);
	}
}

DWORD WINAPI Initialize(LPVOID lpdwThreadParam) {
	HINSTANCE krnlhinst;
	HINSTANCE shell32hinst;

	mutex = CreateMutex(NULL, FALSE, "osu!mutex");
	fileMap = CreateFileMapping(0, NULL, PAGE_READWRITE, 0, filemapsize,
			"osu!filemap");
	if (fileMap != 0) {
		mapmemory = MapViewOfFile(fileMap, FILE_MAP_WRITE | FILE_MAP_READ, 0,
				0, 0);
	} else {
		return FALSE;
	}

	krnlhinst = LoadLibraryA("Kernel32.dll");
	CreateFileWptr = reinterpret_cast<void*> (GetProcAddress(krnlhinst,
			"CreateFileW"));

	shell32hinst = LoadLibraryA("Shell32.dll");
	ShellExecuteExWptr = reinterpret_cast<void*> (GetProcAddress(shell32hinst,
			"ShellExecuteExW"));

	// Initialize MinHook.
	if (MH_Initialize() != MH_OK) {
		return FALSE;
	}

	// Create a hook in disabled state.
	if (MH_CreateHook(CreateFileWptr,
			reinterpret_cast<void*> (&DetourCreateFileW),
			reinterpret_cast<void**> (&fpCreateFileW)) != MH_OK) {
		return FALSE;
	}

	// Enable the hook.
	if (MH_EnableHook(CreateFileWptr) != MH_OK) {
		return FALSE;
	}

	// Create a hook in disabled state.
	if (MH_CreateHook(ShellExecuteExWptr,
			reinterpret_cast<void*> (&DetourShellExecuteExW),
			reinterpret_cast<void**> (&fpShellExecuteExW)) != MH_OK) {
		return FALSE;
	}

	// Enable the hook.
	if (MH_EnableHook(ShellExecuteExWptr) != MH_OK) {
		return FALSE;
	}

	disabled = FALSE;
	return 0;
}

DWORD WINAPI UnInitialize(LPVOID lpdwThreadParam) {
	if (!disabled) {
		// Disable the hook
		if (MH_DisableHook(CreateFileWptr) != MH_OK) {
			return FALSE;
		}

		// Uninitialize MinHook.
		if (MH_Uninitialize() != MH_OK) {
			return FALSE;
		}
	}
	return 0;
}

extern "C" __declspec(dllexport)
DWORD WINAPI DllMain(HINSTANCE hInst, DWORD dwReason, LPVOID lpReserved) {

	// Perform actions based on the reason for calling.
	switch (dwReason) {
	case DLL_PROCESS_ATTACH:

		CreateThread(0, 0, &Initialize, 0, 0, 0);
		// Initialize once for each new process.
		// Return FALSE to fail DLL load.
		break;

	case DLL_PROCESS_DETACH:
		// Perform any necessary cleanup.
		UnInitialize(NULL);

		break;
	}
	return TRUE;
}

