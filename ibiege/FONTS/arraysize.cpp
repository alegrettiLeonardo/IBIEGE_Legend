#include <stdio.h>

int main(int argc, char *argv[])
{
	int j[15];
	printf("number of items in j[] %d",sizeof(j)/sizeof(int));
	
	if (1<2<3){
    printf("\n 1<2<3");
	}
	else
		printf("\n 1<2<1 ERR");

  printf("\n");
	return 0;
}
