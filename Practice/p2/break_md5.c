#include <sys/types.h>
#include <openssl/md5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>

#define PASS_LEN 6
#define N_THREADS 20
#define PBSTR "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
#define PBWIDTH 60

struct data {
    pthread_mutex_t *mutex;
    pthread_mutex_t *mutex_try;
    char **md5;
    int found;
    int attempts;
    int hashes;
};

struct break_md5 {
    int id;
    struct data *data;
};

struct thread_info {
    pthread_t thread;
    struct break_md5 *args;
};

long ipow(long base, int exp)
{
    long res = 1;
    for (;;)
    {
        if (exp & 1)
            res *= base;
        exp >>= 1;
        if (!exp)
            break;
        base *= base;
    }

    return res;
}

long pass_to_long(char *str) {
    long res = 0;

    for(int i=0; i < PASS_LEN; i++)
        res = res * 26 + str[i]-'a';

    return res;
};

void long_to_pass(long n, unsigned char *str) {  // str should have size PASS_SIZE+1
    for(int i=PASS_LEN-1; i >= 0; i--) {
        str[i] = n % 26 + 'a';
        n /= 26;
    }
    str[PASS_LEN] = '\0';
}

int hex_value(char c) {
    if (c>='0' && c <='9')
        return c - '0';
    else if (c>= 'A' && c <='F')
        return c-'A'+10;
    else if (c>= 'a' && c <='f')
        return c-'a'+10;
    else return 0;
}

void hex_to_num(char *str, unsigned char *hex) {
    for(int i=0; i < MD5_DIGEST_LENGTH; i++)
        hex[i] = (hex_value(str[i*2]) << 4) + hex_value(str[i*2 + 1]);
}

void *break_pass(void *ptr) {
    struct break_md5 *args = ptr;
    unsigned char res[MD5_DIGEST_LENGTH];
    char hex[MD5_DIGEST_LENGTH * 2 + 1], *aux;
    unsigned char *pass = malloc((PASS_LEN + 1) * sizeof(char));
    long bound = ipow(26, PASS_LEN); // we have passwords of PASS_LEN
                                     // lowercase chars =>
                                    //     26 ^ PASS_LEN  different cases
    for(long i=args->id; i < bound; i += N_THREADS) {
        pthread_mutex_lock(args->data->mutex);
        if (args->data->hashes != args->data->found) {
            pthread_mutex_unlock(args->data->mutex);

            pthread_mutex_lock(args->data->mutex_try);
            args->data->attempts++;
            pthread_mutex_unlock(args->data->mutex_try);

            long_to_pass(i, pass);

            MD5(pass, PASS_LEN, res);

            hex_to_num(res, hex);


            pthread_mutex_lock(args->data->mutex);
            for (int i = 0; i < args->data->hashes; i++) {
                if(!strcmp(hex, args->data->md5[i])) {
                    printf("\r%s: %-37s\n", args->data->md5[i], pass);
                    for (int j = i; j < (args->data->hashes - 1); j++) {
                        aux = args->data->md5[j];
                        args->data->md5[j] = args->data->md5[j + 1];
                        args->data->md5[j + 1] = aux;
                    }

                    args->data->hashes--;

                    break; // Found it!
                }
            }
            pthread_mutex_unlock(args->data->mutex);
        } else {
            pthread_mutex_unlock(args->data->mutex);
            break;
        }

    }

    free(pass);

    return NULL;
}

void speed(struct data *data){
    pthread_mutex_lock(data->mutex_try);
    int buffer1=data->attempts;
    pthread_mutex_unlock(data->mutex_try);
    usleep(500000);
    pthread_mutex_lock(data->mutex_try);
    int buffer2=data->attempts;
    pthread_mutex_unlock(data->mutex_try);
    printf("\r %d h/s\n", (buffer2-buffer1)*2);
    usleep(10);
}

void *progress(void *ptr) {
    struct break_md5 *args = ptr;
    double attempts = 0;
    long bound = ipow(26, PASS_LEN);
    int found = -1;

    while (found!=0) {
        pthread_mutex_lock(args->data->mutex);
        found = args->data->hashes;
        pthread_mutex_unlock(args->data->mutex);
        pthread_mutex_lock(args->data->mutex_try);
        attempts = (double) args->data->attempts;
        pthread_mutex_unlock(args->data->mutex_try);
    
        int lpad = (int) (attempts/bound * PBWIDTH);
        int rpad = PBWIDTH - lpad;
        printf("\r%3.2f%% [%.*s%*s]\n", 100.0*attempts/bound, lpad, PBSTR, rpad, "");
        usleep(10);
        speed(args->data);
        fflush(stdout);
    }
    return NULL;
}

void init_data(struct data *data, char *md5[], int hashes) {
    data->mutex_try = malloc(sizeof(pthread_mutex_t));
    data->mutex = malloc(sizeof(pthread_mutex_t));
    data->md5 = malloc(sizeof(char*) * hashes);
    pthread_mutex_init(data->mutex_try, NULL);
    pthread_mutex_init(data->mutex, NULL);
    for (int i = 0; i < hashes; i++){
        data->md5[i] =md5[i + 1];
    }
    data->found = 0;
    data->attempts = 0;
    data->hashes=hashes;
}

struct thread_info *start_threads(struct data *data) {
    struct thread_info *threads;

    threads = malloc(sizeof(struct thread_info) * (N_THREADS + 1));

    for (long i = 0; i < N_THREADS + 1; i++) {
        threads[i].args = malloc(sizeof(struct break_md5));
        threads[i].args->id = i;
        threads[i].args->data = data;

        pthread_create(&threads[i].thread, NULL, break_pass, threads[i].args);
        pthread_create(&threads[i].thread, NULL, progress, threads[i].args);
    }
    return threads;
}

void waits(struct thread_info *threads, struct data *data) {
    for (int i = 0; i < N_THREADS + 1; i++)
        pthread_join(threads[i].thread, NULL);

    for (int i = 0; i < N_THREADS + 1; i++)
        free(threads[i].args);

    pthread_mutex_destroy(data->mutex_try);
    pthread_mutex_destroy(data->mutex);
    free(data->mutex_try);
    free(data->md5);
    free(data->mutex);
    free(threads);
}

int main(int argc, char *argv[]) {
    struct thread_info *thrs;
    struct data data;

    if(argc < 2) {
        printf("Use: %s string\n", argv[0]);
        exit(0);
    }
    init_data(&data, argv, --argc);

    thrs = start_threads(&data);

    waits(thrs, &data);

    return 0;
}
