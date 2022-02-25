# Exercices

## Exercice (b)
Using  MAX_CARS global value

## Exercice (c)
Adding semaphores

```c
mutex_t bridge;
int cars[2];
mutex_t cars_m[2]
cond_t bridge_in_use[2]
sem_t bridge_capacity[2]={max_cars, max_cars}
```

```c
enter_bridge(int direction){
    lock(bridge);
    cars[direction]++;
    while(cars[direction] ==max_cars && cars[(direction+1)%2]>0){
        wait(bridge_in_use[direction], bridge); //1
    }
    cars[direction]++,
    unlock(bridge);
    P(bridge_capacity[direction]);
}//cars crossing
```

```c
exit_bridge(int direction){
    lock(bridge);
    cars[direction]--;
    if(cars[directions]==0){
        broadcast(bridge_in_use[(direction+1)%2]);
        unlock(bridge); //0
    }
    if(cars[directions]==max_cars-1){
        broadcast(bridge_in_use[direction]);
    }
    unlock(bridge);
    V(cars[direction]);
}
```

# Exercice 2
## group visits

```c
struct group{
    const int members[1];
    pthread_mutex_t *counter_m;
    int counter;
};

int capacity;

void *visitor(void *arg){
    struct group *grp = arg;

    //Access 

    visit();

    //Exit
}
```

# Exercice 3

Exercise synchronized computation
    -Always with void* param and args
    -calls force(future), + function free_future to free memory
***Example***

```c
struvt arg{
    int i;
    int result;
}
```