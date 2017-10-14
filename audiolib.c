#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <vlc/vlc.h>

void play(char * path) {
    printf("start playing\n");
    libvlc_instance_t *inst;
    libvlc_media_player_t *mp;
    libvlc_media_t *m;

    // load the vlc engine
    inst = libvlc_new(0, NULL);

    // create a new item
    m = libvlc_media_new_path(inst, path);

    // create a media play playing environment
    mp = libvlc_media_player_new_from_media(m);

    libvlc_media_parse(m);

    // no need to keep the media now
    libvlc_media_release(m);

    libvlc_media_player_play(mp);

    unsigned long long duration = libvlc_media_get_duration(m);
    printf("duration %llu\n", duration);
    time_t now = time(0);
    while (time(0) <= now + (duration / 1000)) {
         
    }

    // stop playing
    libvlc_media_player_stop(mp);

    // free the media_player
    libvlc_media_player_release(mp);

    libvlc_release(inst);
    printf("finished\n");
}

/*int main(int x, char * argv[]) {*/
    /*play(argv[1]);*/
    /*return 0;*/
/*}*/
