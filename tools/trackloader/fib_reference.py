"""Dependency-free reference decoder for the disk build's FIB1 banks."""
import struct
DELTA=(-34,-21,-13,-8,-5,-3,-2,-1,0,1,2,3,5,8,13,21)
def fib_decode(data):
    magic,count,blocksize=struct.unpack_from('<4sII',data)
    if magic!=b'FIB1' or blocksize!=512:
        raise ValueError('Unsupported Fibonacci bank')
    output=bytearray();pos=12
    while len(output)<count:
        n=min(blocksize,count-len(output))
        size=1+n//2
        if pos+size>len(data):raise ValueError('Truncated Fibonacci block')
        pred=data[pos];pos+=1;output.append(pred)
        for i in range(n-1):
            code=(data[pos+i//2]>>(4 if i%2==0 else 0))&15
            pred=(pred+DELTA[code])&255;output.append(pred)
        pos+=n//2
    if pos!=len(data):raise ValueError('Trailing Fibonacci data')
    return bytes(output)
