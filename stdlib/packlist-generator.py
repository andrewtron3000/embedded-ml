import sys

# packlist file format
# type name
# recordname size
# ...

if len(sys.argv) < 2:
    print 'Usage: %s filename' % sys.argv[0]
    exit(1)

def printRecord(rec, others, typ, siz):
    s = '('
    s = s + 'fn r => #%s/%s r,\n' % (rec, typ)
    s = s + ' (fn r v => { %s = v,\n' % rec
    others = [x for x in others if x != rec]
    num_others = len(others)
    for (i, o) in enumerate(others):
        s = s + '              %s = #%s/%s r' % (o, o, typ)
        if i < num_others - 1:
            s = s + ',\n'
        else:
            s = s + '}),\n'
    s = s + (' %s)' % siz)
    return s


filename = sys.argv[1]

f = open(sys.argv[1])
ls = f.readlines()
num_record_elements = len(ls) - 1

typename = ls[0].strip()
records = []
sizes = []

for l in ls[1:]:
    (r, s) = l.split()
    records.append(r.strip())
    sizes.append(s.strip())

print 'val %s-packlist =' % typename
num_records = len(ls) - 1
for (i, r) in enumerate(records):
    print printRecord(r, records, typename, sizes[i]),
    if i < num_records - 1:
        print '::\n'
    else:
        print ':: nil\n'
        

