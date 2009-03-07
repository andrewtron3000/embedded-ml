import sys
# this function generates a record that represents state, along with state accessor functions.

# ams
# roleID, int
# unitID, int
# continuumID, int

# results in
#
# type ams-recordtype =
#     {
#         continuumid : int,
#         roleid : int,
#         unitid : int
#     }
#
# datatype ams-statetype =
#     continuumID of int |
#     roleID of int |
#     unitID of int
#
# fun ams-update-state tv st =
#     case tv of 
#         continuumID v => {
#             continuumid = v,
#             roleid = #roleid/ams-recordtype st,
#             unitid = #unitid/ams-recordtype st
#          }
#         | roleID v => {
#             continuumid = #continuumid/ams-recordtype st,
#             roleid = v,
#             unitid = #unitid/ams-recordtype st
#          }
#         | unitID v => {
#             continuumid = #continuumid/ams-recordtype st,
#             roleid = #roleid/ams-recordtype st,
#             unitid = v
#          }
# queries can be performed by using the record selectors


class outputEngine:
    def __init__(self, indent=0, dx=4):
        self.indentLevel = indent
        self.dx = dx
        self.s = ''
    def getIndent(self):
        return self.indentLevel
    def increaseIndent(self):
        self.indentLevel += self.dx
    def decreaseIndent(self):
        self.indentLevel -= self.dx
    def add(self, l):
        ls = l.split('\n')
        for l in ls:
            if l != '':
                indent = ' ' * self.indentLevel
                self.s += indent + l + '\n'
    def interleave(self, ls, x):
        t = ''
        for l in ls[0:len(ls)-1]:
            t += l
            t += x
        t += ls[len(ls)-1]
        self.add(t)
    def dump(self):
        r = self.s
        self.s = ''
        return r

def getType(d, x):
    return d[x][0]

def importRecords(ls):
    ''' ls specifies the packet structures '''
    d = {}
    keys = []
    for l in ls:
        l = l.strip()
        xs = l.split(',')
        xs = map( lambda x: x.strip(), xs )
        name = xs[0]
        d[name] = xs[1:]
        keys.append(name)
    return (d, keys)

def createDatatypeDefinition(d, prefix):
    oe = outputEngine()
    oe.add("datatype %s-optype =\n" % prefix)
    oe.increaseIndent()
    zs = []
    for x in d.keys():
        t = getType(d, x)
        zs.append('%s of %s' % (x, t))
    oe.interleave(zs, ' |\n')
    oe.add('\n')
    oe.decreaseIndent()
    return oe.dump()

def createRecordDefinition(d, prefix):
    oe = outputEngine()
    oe.add("type %s-statetype =\n" % prefix)
    oe.increaseIndent()
    oe.add("{\n")
    oe.increaseIndent()
    zs = []
    for x in d.keys():
        t = getType(d, x)
        zs.append('%s : %s' % (x.lower(), t))
    oe.interleave(zs, ',\n')
    oe.add('\n')
    oe.decreaseIndent()
    oe.add("}\n")
    oe.decreaseIndent()
    return oe.dump()

def recordPicker(d, prefix, y):
    oe = outputEngine()
    oe.increaseIndent()
    es = []
    for x in d.keys():
        if x == y:
            es.append('%s = v' % x.lower())
        else:
            es.append('%s = #%s/%s-statetype st' % (x.lower(), x.lower(), prefix))
    oe.interleave(es, ',\n')
    oe.decreaseIndent()
    return oe.dump()

def createUpdateDefinition(d, prefix):
    oe = outputEngine()
    oe.add("fun %s-update-state tv st =\n" % prefix)
    oe.increaseIndent()
    oe.add("case tv of ")
    oe.increaseIndent()
    zs = []
    for x in d.keys():
        s = '%s v => {\n' % x
        s += recordPicker(d, prefix, x)
        s += ' }'
        zs.append(s)
    oe.interleave(zs, '\n| ')
    oe.add('\n')
    oe.decreaseIndent()
    return oe.dump()

def literal(s):
    return (s[0] == '>')

def notliteral(s):
    return not (literal(s))

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit('usage: %s inputfile\n' % sys.argv[0])
    
    f = open(sys.argv[1], 'r')
    ls = f.readlines()
    f.close()

    # print the raw code
    cs = filter(literal, ls)
    for c in cs:
        print c[1:],

    # now handle the non literal code, lines that don't start with '>'
    ls = filter(notliteral, ls)
    prefix = ls[0].strip()
    d, ks = importRecords(ls[1:]) 
    print createRecordDefinition(d, prefix)
    print createDatatypeDefinition(d, prefix)
    print createUpdateDefinition(d, prefix)
