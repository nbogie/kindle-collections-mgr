#!/usr/bin/env python
#from http://code.google.com/p/kcollect/ 
import json
import os
import hashlib
import re
import sys
import shutil
import time

from optparse import OptionParser
HASH_PREFIX = "/mnt/us/documents/"
#HASH_PREFIX = "/media/Kindle/documents/"
COLLECTIONS = None
VERBOSE = False

def compute_hash(path):
    hin = HASH_PREFIX + path
    debug("Computing hash of %s"%hin)
    hobj = hashlib.sha1(hin)
    hval = hobj.hexdigest()
    debug("Hash computed %s for %s"%(hval, hin))
    return hval

def collections(root):
    if not COLLECTIONS is None:
        return COLLECTIONS

    return root + "/system/collections.json"
    
def complain(err):
    print "Warning: %s"%err

def die(err):
    print "Error: %s"%err
    sys.exit(1)

def debug(str):
    if VERBOSE:
        print str

class collection(object):
    def __init__(self, root):
        self.root_ = root
        self.init()

    def init(self):
        self.data_={}             # Raw data (keys are names, vals are lists of hashes)
        self.collections_ = {}    # Keys are names, vals are lists of files
        self.hashes_ = {}         # map of hashes to filenames

    def load(self):
        # Read the list of all files and create a hash map
        debug("loading from root %s"%self.root_)
        for root, dirs, files in os.walk(self.root_ + "/documents"):
            debug("root = %s"%root)
            for f in files:
                debug("Found file on filesys %s"%f)
                self.hashes_[compute_hash(f)] = f
        
        # Read the collections file. This contains hashes
        debug("Loading file %s"%collections(self.root_))

        f = file(collections(self.root_), "r")

        self.raw_ = json.load(f)
        for k in self.raw_:
            if k == 'lastAccess':
                continue
            
            self.data_[k] = self.raw_[k]['items']
            lst = []
            for it in self.data_[k]:
                m = re.match("\*([a-z0-9]+)$", it)
                if m is None:
                    die("Bogus hash %s"%it)
                
                hash = m.group(1)

                if not hash in self.hashes_:
                    complain("Hash %s does not match any file"%hash)
                    continue
                
                lst.append(self.hashes_[hash])
                
                self.collections_[k] = lst
            

    def save(self):
        js = {}

        for k in self.collections_:
            js[k] = {}
            js[k]["lastAccess"] = int(time.time() * 1000)
            js[k]["items"] = []

            for f in self.collections_[k]:
                js[k]["items"].append("*" + compute_hash(f))
        
        shutil.copy(collections(options.root), collections(options.root) + ".bak")
        fp = file(collections(options.root), "w")
        json.dump(js, fp)
        fp.write("\n")
        fp.close()

    def new(self, collection):
        if collection in self.collections_:
            die("Collection %s already exists"%collection)

        self.collections_.append(collection)

        
    def add(self, collection, file):
        if not collection in self.collections_:
            self.collections_[collection] = []
            
        if file in self.collections_[collection]:
            complain("File %s already in collection %s"%(file, collection))
            return

        self.collections_[collection].append(file)

    def remove(self, collection, f):
        if not collection in self.collections_:
            die("Collection %s does not exist"%collection)
        
        if not f in self.collections_[collection]:
            die("File %s not in collection %s"%(f, collection))
        
        while f in self.collections_[collection]:
            self.collections_[collection].remove(f)
        

    def dump(self):
         for k in self.collections_:
             print k
             for v in self.collections_[k]:
                 print "  %s"%v
             print

    def undump(self, path):
         self.init()
         f = file(path, "r")
         k = None

         for l in f:
             if l == "":
                 continue

             m = re.match("\s+([\S].*)", l)
             if m is None:   # collection name
                 k = l
                 self.collections_[k] = []
             else:
                 if k is None:
                     die("Invalid format %s"%l)
                 else:
                     self.collections_[k].append(m.group(1))


parser = OptionParser()
parser.add_option("-r", "--root", dest="root", default="/Volumes/Kindle", help="Point to new kindle root directory")
parser.add_option("-c", "--collections", dest="collections", default=None, help="Use a different collections file")
parser.add_option("-v", "--verbose", dest="verbose", default=False, help="Verbose mode")

(options, args) = parser.parse_args()

    
if options.verbose:
    VERBOSE = True

# Default is just to list
if len(args) == 0:
    args = ["list"]

# First read the collection
col = collection(options.root)
col.load()

# Now do stuff
action = args.pop(0)

if action == "list":
    col.dump()    

elif action == "add":   # <collection> <file> ...
    # 
    collection = args.pop(0)

    if len(args) == 0:
        die("No files to add")

    for f in args:
        bn = os.path.basename(f)
        if not os.path.exists(options.root + "/documents/" + bn):
            if not os.path.exists(f):
                die("File %s does not exist"%f)
            shutil.copy(f, options.root + "/documents/" + bn)
        col.add(collection, bn)

    col.save()

elif action == "save": # For testing. Mostly useless
    col.save()

elif action == "remove": # <collection> <file> ..
    # 
    collection = args.pop(0)

    if len(args) == 0:
        die("No files to add")

    for f in args:
        col.remove(collection, os.path.basename(f))
    col.save()

elif action == "rmcol": # <collection>
    #
    die("Unimplemented")

else:
    die("Action %s is unknown"%action)

