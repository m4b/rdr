#!/usr/bin/env python3

import re
import sys

constre = re.compile("(?P<name>\w+)\s+(?P<const>\w+)")

f = open (sys.argv[1])
content = f.read()
f.close()

def getTypeDef(content):
    name = content[0][0]
    typ = content[0][1]
    res = "type "+name.lower()+" =\n"
    for constructor, const in content[1:]:
        res+= "    | "+constructor+"\n"
    res+="    | UNKNOWN of "+typ+"\n"
    return res

def getConstToName(content):
    name = content[0][0]
    typ = content[0][1]
    res = "let get_"+name+" =\n  function\n"
    for constructor,const in content[1:]:
        res+= "  | "+const+" -> "+constructor+"\n"
    res+="  | x -> UNKNOWN x\n"
    return res

def getNameToConst(content):
    name = content[0][0]
    typ = content[0][1]
    res = "let "+name+"_to_int =\n  function\n"
    for constructor,const in content[1:]:
        res+= "  | "+constructor+" -> "+const+"\n"
    res+="  | UNKNOWN x -> x\n"
    return res

def getPrinter(content):
    name = content[0][0]
    typ = content[0][1]
    res = "let "+name+"_to_string =\n  function\n"
    for constructor,const in content[1:]:
        res+= "  | "+constructor+" -> \""+constructor+"\"\n"
    res+="  | UNKNOWN x -> Printf.sprintf \"UNKNOWN_"+name.upper()+" 0x%x\" x\n"
    return res

def get(program):
    nodes = []
    tokens = constre.findall(program)
    nodes.append(getTypeDef(tokens))
    nodes.append(getConstToName(tokens))
    nodes.append(getNameToConst(tokens))
    nodes.append(getPrinter(tokens))
    return nodes

def printOcamlModule (nodes):
    for node in nodes:
        print (node)

nodes = get(content)

printOcamlModule(nodes)
