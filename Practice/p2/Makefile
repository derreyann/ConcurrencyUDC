CFLAGS=-g -Wall -pthread -I/opt/homebrew/Cellar/openssl@3/3.0.1/include
LDFLAGS=-lcrypto -L/opt/homebrew/Cellar/openssl@3/3.0.1/lib

all: break_md5

break_md5: break_md5.c
	gcc break_md5.c -o break_md5 $(CFLAGS) $(LDFLAGS)

clean:
	rm -f *.o break_md5
