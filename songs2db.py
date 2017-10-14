import MySQLdb
import hashlib

from os import listdir, rename
from os.path import isfile, join

if __name__ == '__main__':
    db = MySQLdb.connect(host="localhost",
                         user="muscom",
                         passwd="muscom00muscom",
                         db="muscom")
    cur = db.cursor()

    path = 'music'
    files = [f for f in listdir(path) if isfile(join(path, f))
             and f.endswith('.mp3')]
    print("files ", files)
    for f in files:
        title = f.split('.mp3')[0]
        new_path = hashlib.md5(
            title.encode("utf-8")).hexdigest() + ".mp3"
        print("title", title, "path", new_path)
        rename(join(path, f), join(path, new_path))
        cur.execute('INSERT INTO songs (title, path) VALUES ("{}", "{}")'
                    .format(title, new_path))

    db.commit()
    db.close()
