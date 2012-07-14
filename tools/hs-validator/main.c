#include "libvm.c"
#include <limits.h>

//reverse string (str, 0, strlen-1)
void reverse(char *x, int beg, int end){
   char c;
   if ( beg >= end )
      return;    
   c = *(x+beg);
   *(x+beg) = *(x+end);
   *(x+end) = c;
   reverse(x, ++beg, --end);
}

void anyMove(struct state *s, char **a, int stage){
	int k,is,js;
	char *answer = malloc( (2)*sizeof( char ));
	struct state *t=copy(s);
	answer[1]='\0';
	for(k=0; k<=25 ; k++){
		if((k+stage)%5==0){answer[0]='R';is=-1; js=0;}
		if((k+stage)%5==1){answer[0]='L';is=1; js=0;}						
		if((k+stage)%5==2){answer[0]='U';is=0; js=-1;}
		if((k+stage)%5==3){answer[0]='D';is=0; js=1;}
		if(k==4){answer[0]='W';is=0; js=0;}
//		if(k<5 && s->robot_y+js<s->world_h&& s->robot_x+is<s->world_w && get(s,s->robot_x+is, s->robot_y+js) == O_WALL)
//			continue;
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
    int i,j, is, js, k, wx=0, wy=0, stage=-1;
	unsigned int change = 0, end = 0;
    struct state *ns;
	char * answer;
	char move='U';
	unsigned int *c = malloc( ((s-> world_w+1) * s->world_h+1) * sizeof( unsigned int ));
	
	if (c == NULL ){}
	for(i=0; i<((s-> world_w+1) * s->world_h+1); i++){
		c[i]=UINT_MAX;  
	}
	
	c[unmake_point(s, s->robot_x, s->robot_y)]=0;	
	
	put(s, 1, 1, O_EMPTY);
	move_robot(s, 1, 1);
	ns = copy(s);
	
	do{
	    free(s);
	    s = copy(ns);
		update_world(ns, s);
		change++;
		stage++;
		end = 0;
		for(i=1; i <= s->world_w; i++){
			for(j=1; j <= s->world_h; j++){
				if(c[unmake_point(s,i,j)]==stage){
					if(get(s, i, j) == O_OPEN_LIFT || get(s, i, j) == O_LAMBDA){
						wx=i;
						wy=j;
						end=stage;
						break;
					}
					//consider four cells - (-1, 0), (1, 0), (0, -1), (0, 1)
					for(k = 1; k<=4; k++){
						if(k==1) {is= 0 ; js=-1;}
						if(k==2) {is= 0 ; js= 1;}
						if(k==3) {is=-1 ; js= 0;}
						if(k==4) {is= 1 ; js= 0;}						
						if(i+is>0 && j+js > 0
							&& i+is <= s->world_w && j+js <= s->world_h 
							&& c[unmake_point(s, i+is, j+js)]==UINT_MAX){
							//can we go there?
							if(	j+js+1 < s->world_h && (get(s, i+is, j+js+1) != O_EMPTY || get(ns, i+is, j+js+1) != O_ROCK)){	
								if(get(s, i+is, j+js)==O_OPEN_LIFT 
								|| get(s, i+is, j+js)==O_LAMBDA 
								|| get(s, i+is, j+js)==O_EARTH 
								|| get(s, i+is, j+js)==O_EMPTY){
									if((j+js+1 < s->world_h && get(s, i+is, j+js+1)==O_ROCK)
										|| (j+js+1 <= s->world_h && i+is+1 <= s->world_h && get(s, i+is+1, j+js)==O_ROCK && get(s, i+is+1, j+js+1)==O_ROCK)
										|| (j+js+1 <= s->world_h && i+is-1 > 0 && get(s, i+is-1, j+js)==O_ROCK && get(s, i+is-1, j+js+1)==O_ROCK)){
										c[unmake_point(s, i+is, j+js)]=stage+penalty;
									}else{
										c[unmake_point(s, i+is, j+js)]=stage+1;
									}
									change = 0;
							    }
							}
						}
					}
				}
			}
		}
	}while(change <= penalty && end == 0);
	
	//debug
	dump(s);
	
	if(1){
		for(j=s->world_h; j>0; j--){
			for(i=1; i<=s->world_w; i++){
				if(c[unmake_point(s, i, j)]==UINT_MAX){
					printf("X");
				}else{
					printf("%u", c[unmake_point(s, i, j)]);
				}
			}
			printf("\n");
		}
		printf("\n");
	}
	
	i=0;
	if(end>0){
		answer = malloc( (end+2)*sizeof( char ));
		if (answer == NULL ){}
		
		while(end>0){
			for(k=1; k<=4;k++){
				if(k==1) {is=-1; js=0;move='R';}
				if(k==2) {is=1; js=0;move='L';}						
				if(k==3) {is=0; js=-1;move='U';}
				if(k==4) {is=0; js=1;move='D';}
				if( wx+is > 1 && wy+js > 1 
				 && wx+is < s->world_w && wy+js < s->world_h 
				 && c[unmake_point(s, wx+is, wy+js)] < end) {
					end = c[unmake_point(s, wx+is, wy+js)];
					answer[i++]=move;
					wx = wx+is;
					wy = wy+js;
					break;
				}
			}
		}
		answer[i]='\0';
		reverse(answer,0,strlen(answer)-1);
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
	printf("%c",  get(s, 1,1));
	struct state *t, *s0;
	char *answer;
	int status, stage=0;
	char *result = malloc (((s-> world_w+1) * s->world_h * sizeof(char)));

	strcpy(result, "");
	s0 = copy(s);
	srand(time(NULL));
	
	do{
		t = copy(s);
		status=goSomewhere(t, &answer, 30);
		free(t);
		t = copy(s);
		if(status==0) 
			s = make_moves(s, answer);
		if(status==1 || s->condition == C_LOSE){
			free(s);
			s=copy(t);
			anyMove(t, &answer, rand()*10);
			stage++;
     		s = make_moves(s, answer);
		}
		strcat(result, answer);
		free(t);
	}while(s->condition == C_NONE && stage<s->world_h*8);

	s = make_moves(s0, result);
	dump(s);
	printf("%s", result);	
	return 0;
}
