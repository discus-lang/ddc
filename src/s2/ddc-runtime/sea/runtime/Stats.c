#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

typedef struct TimeDiff{
  struct timeval start;
  struct timeval end;
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
      fprintf(stderr,"Erro on Cache. start\n");
  }
  cache = (TimeDiff*)malloc(sizeof(TimeDiff));
  gettimeofday(&cache->start, NULL);
}

void ddcSeaStatsGCEnd(){
  if(cache == NULL){
      fprintf(stderr,"Erro on Cache. end \n");
      return;
  }
  gettimeofday(&cache->end, NULL);

  if(timeList == NULL){
    timeList = (Node*)malloc(sizeof(Node));
    timeList->next = NULL;
    timeList->data = cache;
  }else{
    append(timeList, cache);
    fprintf(stderr,",%lu", (unsigned long)(cache->end.tv_usec - cache->start.tv_usec));
  }
  cache = NULL;
}


void ddcSeaStatsShowTimeDiff(){
  Node* node = timeList;
  while(node != NULL){
    TimeDiff* diff = (TimeDiff*)(node->data);
    fprintf(stderr,",%lu", (unsigned long)(diff->end.tv_usec - diff->start.tv_usec));
    node = node->next;
  }
  fprintf(stderr, "\n");

  destroyList(timeList);
}



