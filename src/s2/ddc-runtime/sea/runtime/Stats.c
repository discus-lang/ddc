#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct TimeDiff{
  clock_t start;
  clock_t end;
} TimeDiff;

typedef struct Node {
  struct Node* next;
  void* data;
} Node;



TimeDiff* cache;
Node* timeList;


Node* createList(){
  Node* rVal = (Node*) malloc(sizeof(Node));
  rVal->next = NULL;
  rVal->data = NULL;
  return rVal;
}

void destroyList(Node* node){
  Node* prev = NULL;
  while(node != NULL){
    free(node->data);
    prev = node;
    node = node->next;
    free(prev);
  }
}

void append(Node* node, void* data){
  if(node == NULL){
    return;
  }
  while(node->next != NULL){
    node = node->next;
  }
  node->next = malloc(sizeof(Node));
  node->next->next = NULL;  
  node->next->data = data;
}

void ddcSeaStatsGCStart(){
  if(cache != NULL){
      fprintf(stdout,"Erro on Cache. start\n");
  }
  cache = (TimeDiff*)malloc(sizeof(TimeDiff));
  cache->start = clock();

}

void ddcSeaStatsGCEnd(){
  if(cache == NULL){
      fprintf(stdout,"Erro on Cache. end \n");
      return;
  }
  cache->end = clock();

  if(timeList == NULL){
    timeList = (Node*)malloc(sizeof(Node));
    timeList->next = NULL;
    timeList->data = cache;
  }else{
    append(timeList, cache);
    double timeTaken = (((double)(cache->end -  cache->start))/CLOCKS_PER_SEC) * 1000;
    fprintf(stderr,",%f", timeTaken);
  }
  cache = NULL;
}


void ddcSeaStatsShowTimeDiff(){
  Node* node = timeList;
  while(node != NULL){
    TimeDiff* diff = (TimeDiff*)(node->data);
    double timeTaken = (((double)(diff->end -  diff->start))/CLOCKS_PER_SEC) * 1000;
    fprintf(stderr,",%f", timeTaken);

    node = node->next;
  }
  fprintf(stderr, "\n");

  destroyList(timeList);
}



