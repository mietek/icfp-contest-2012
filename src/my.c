#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "libvm.c"

//reverse string (str, 0, strlen-1)
void reverse(char *s) {
    int i, j;
    char c;
    for (i = 0, j = strlen(s) - 1; i < j; i++, j--) {
        c = s[i];
        s[i] = s[j];
        s[j] = c;
    }
}


void anyMove(struct state *s, char **a, int stage){
	int k,is,js;
	char *answer = calloc(2, sizeof(char));
	struct state *t=copy(s);
	answer[1]='\0';
	for(k=0; k<=5 ; k++){
		if((k+stage)%5==0){answer[0]='R';is=-1; js= 0;}
		if((k+stage)%5==1){answer[0]='L';is= 1; js= 0;}
		if((k+stage)%5==2){answer[0]='U';is= 0; js=-1;}
		if((k+stage)%5==3){answer[0]='D';is= 0; js= 1;}
		if(k==4)          {answer[0]='W';is= 0; js= 0;}
		if(k==5) answer[0]='A';
		t = make_moves(s, answer);
		if(t->condition == C_NONE)
			break;
    }
	free(t);
	*a=answer;
	return;
}

int goSomewhere(struct state *s, char **a, int penalty){
    long i,j, is, js, k, wx=0, wy=0, best, end;
    struct state *ns;
	char *answer;
	char move='U';

	long * c;
	struct cost_table *nc;

	nc = build_cost_table(s, s->robot_x, s->robot_y);

	// find the best goal
	best=LONG_MAX;

	for(j=s->world_h; j>0; j--){
		for(i=1; i<=s->world_w; i++){
			get_cost(nc, i, j)==LONG_MAX ? printf("X"):printf("%u", get_cost(nc, i, j));
			if(safe_get(s, i, j)==O_LIFT_OPEN || safe_get(s, i, j)==O_LAMBDA){
				if(get_cost(nc, i, j)<best){
					best=get_cost(nc, i, j);
					wx=i;
					wy=j;
				}
			}
		}
		printf("\n");
	}
	printf("\n");

	i=0;
	if(best<LONG_MAX){
		answer = malloc( (best+2)*sizeof( char ));
		if (answer == NULL ){}

		while(best>0){
			for(k=1; k<=4;k++){
				if(k==1) {is=-1; js= 0;move='R';}
				if(k==2) {is= 1; js= 0;move='L';}
				if(k==3) {is= 0; js=-1;move='U';}
				if(k==4) {is= 0; js= 1;move='D';}
				if(is_within_world(s->world_w, s->world_h, wx+is, wy+js) && get_cost(nc, wx+is, wy+js) < best) {
					best = get_cost(nc, wx+is, wy+js);
					answer[i++]=move;
					wx = wx+is;
					wy = wy+js;
					break;
				}
			}
		}
		answer[i]='\0';
		reverse(answer);
	}else{
		answer = malloc(1*sizeof( char ));
		if (answer == NULL ){}
		answer[0]='\0';
     	*a=answer;
		return 1;
	}
	*a=answer;
	return 0;
}

int main(int argc, char *argv[]){
	struct state *s = new_from_file(argv[1]);
	struct state *t, *s0;
	char *answer;
	int status, stage, i, j, bestv=0;
	char *result = malloc (((s-> world_w+1) * s->world_h * sizeof(char)));
	char *best = malloc (((s-> world_w+1) * s->world_h * sizeof(char)));

	s0 = copy(s);
	srand(time(NULL));

  for(j=1; j<2; j++){
    stage=0;
	strcpy(result, "");
	free(s);
	s = copy(s0);
	i= j%80+1;
	do{
		t = copy(s);
		status=goSomewhere(t, &answer, i);
		free(t);
		t = copy(s);
		if(status==0)
			s = make_moves(s, answer);
		if(status==1 || s->condition == C_LOSE || (j>30 && rand()*100>180-j)){
			free(s);
			s=copy(t);
			anyMove(t, &answer, rand()*5);
			stage++;
     		s = make_moves(s, answer);
		}
		strcat(result, answer);
		free(t);
        dump(s);
	}while(s->condition == C_NONE && stage < s->world_h*8 && s->score>-1000 );

	s = make_moves(s0, result);
	printf("%d: %ld\n", j, s->score);
	if(s->score > bestv){
		strcpy(best, result);
		bestv = s->score;
	}
  }
  s = make_moves(s0, best);
  dump(s);
  printf("%s\n", best);
  fflush(stdout);
  return 0;
}


/*
(07:06:55) jmi-home: y>get_water_level(s)?
(07:08:34) mietek: get_used_robot_waterproofing(s)
(07:08:48) mietek: Jest od 0 do get_robot_waterproofing(s)
*/
