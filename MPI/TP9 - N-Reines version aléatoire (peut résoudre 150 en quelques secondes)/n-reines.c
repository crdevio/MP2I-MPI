#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
bool valid(int n, int tour_joue, int* solutions)
{
    int coup = solutions[tour_joue];
    for(int i = 0; i < tour_joue;i++)
    {
        if(solutions[i]==coup) {return false;}
        if(abs(solutions[i] - coup) == abs(i-tour_joue)) return false;
    }

    return true;


}

void swap(int* tab, int i, int j)
{
    int save = tab[i];
    tab[i] = tab[j];
    tab[j] = save;
}

bool aux(int n, int act, int* solutions)
{
    if(act ==n) return true;
    int t = 0;
    int col_choisie = 0;
    for(int v = 0; v < n; v++)
    {
        solutions[act]=v;
        if(valid(n,act,solutions))
        {
            t++;
            if(rand() % t == 0) {col_choisie = v;}
        }
    }
    if(t == 0) {return false;}
    solutions[act] = col_choisie;
    return aux(n,act+1,solutions);

}

int* nreines(int n)
{
    while(true)
    {
        int* solutions = (int*)(malloc(sizeof(int)*n));
        if(aux(n,0,solutions)) {return solutions;};
        free(solutions);
    }
}

void main()
{
    srand(time(NULL));
    int n =150;
    int* d = nreines(n);
    for(int i = 0; i < n;i++)
    {
        printf("%d ",d[i]);
    }
}