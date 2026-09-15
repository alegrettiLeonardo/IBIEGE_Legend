#include <stdio.h>

/* definição do eixo */
int l_sgm[]={100,80,95,300,95,80,100};
int d_sgm[]={50,80,110,150,110,80,50};
int xi_kg[]={135,375,530};
int l_kg[]={10,100,15};
int kg[]={45,289,60};

/* declaração de funções */
int sh_sgm(int i);
int dv_sgm(int i, int id_kg[]);
int sgm_c_kg(int i, int id_kg[]);

/* globais */
int nc, dv;

/* Programa principal */
int main(int argc, char *argv[])
{
	int i, id_kg[sizeof(xi_kg)/sizeof(int)];
	/* Verifica cada segmento e cada carga */
	for (i=0; i<sizeof(l_sgm)/sizeof(int); i++){
		if (sgm_c_kg(i,id_kg)==1){
			if (dv==1){
				dv_sgm(i,id_kg);
			}
			else {
        sh_sgm(i);
			}
		}
		else {
			sh_sgm(i);
		}
	}
	return 0;
}

/* imprime segmento */
int sh_sgm(int i){
	printf("\n@ sgmt %d : %d",i,l_sgm[i]);
	return 0;
}

/* 
  divide segmento conforme tamanho de sua(s) carga(s)
  e imprime na tela
*/
int dv_sgm(int i, int id_kg[]){
  int bg, ed, j, bgc, edc, i2=0;
	for (j=0; j<i; j++){bg+=l_sgm[j];}
	ed=bg+l_sgm[j];
	j=0;
	do{
		if (j<nc){
      bgc=xi_kg[j];
			edc=bgc+l_kg[j];
			if (bg<bgc){ /* segmento antes de uma carga */
				printf("\n# sgmt %d.%d : %d",i,i2,bgc-bg);
				bg=bgc;
			}
			else { /* segmento de uma carga (distribuída) */
				printf("\n$ sgmt %d.%d : %d",i,i2,edc-bg);
				bg=edc;
				j++;
			}
		}
		else{ /* último segmento */
			printf("\n$ sgmt %d.%d : %d",i,i2,ed-bg);
			bg=ed;
		}
		i2++;
	}
	while (bg<ed);
}

/* 
  verifica se há carga(s) no segmento e retorna o(s)
	respectivo(s) índice(s) no vetor de cargas
*/
int sgm_c_kg(int i, int id_kg[]){
	int j, bg, ed, bgc;
	int b;
	bg=0;
	for (j=0; j<i; j++){
		bg+=l_sgm[j];
		//printf("\n l_sgm[%d] %d",j,l_sgm[j]);
	}
	ed=bg+l_sgm[j];
  //printf("\n bg %d, ed %d",bg,ed);
	nc=0;
	b=0;
	dv=0;
  for (j=0; j<(sizeof(xi_kg)/sizeof(int)); j++){
    bgc=xi_kg[j];
		if (bg<=bgc<=ed){
			b=1;
      id_kg[nc]=j;
			nc++;
			if ((l_kg[j]-bgc)<(bg-ed)) dv=1;
		}
		else if (bgc>ed){break;}
  }
	return b;
}