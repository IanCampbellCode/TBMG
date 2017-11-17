#include <windows.h>

int main(int argc, char *argv[])
{
	char c = argv[1][0];
	int n = c - '0';
	int l = 500;
	if(n == 1){
		//A4
		Beep(440, l);
	} else if (n == 2){
		//B4
		Beep(493.88, l);
	} else if (n == 3){
		//C5
		Beep(523.25, l);
	} else if (n == 4){
		//D5
		Beep(587.33, l);
	} else if (n == 5){
		//E5
		Beep(659.25, l);
	} else if (n == 6){
		//F5
		Beep(698.46, l);
	} else if (n == 7){
		//G5
		Beep(783.99, l);
	} else if(n == 8) {
		//A5
		Beep(880,500);
	}else if(n == 0) {
		//Rest
		Sleep(500);
	}
return 0;
}
