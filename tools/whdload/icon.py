"""Classic Workbench project icon with an original one-bit hexagon image."""
import struct

def launcher_icon(slots):
    # DiskObject (78 bytes), Gadget (44 bytes), Image (20 bytes), BE on disk.
    header=bytearray(78)
    struct.pack_into('>HH',header,0,0xe310,1)
    struct.pack_into('>hhhhHHH',header,8,0,0,32,24,4,3,1)
    struct.pack_into('>I',header,22,1) # GadgetRender present
    header[48]=4                    # WBPROJECT
    struct.pack_into('>IIIIIII',header,50,1,1,0x80000000,0x80000000,0,0,8192)
    image=struct.pack('>hhhhhIBBI',0,0,32,24,1,1,1,0,0)
    pixels=[]
    for y in range(24):
        edge=7+abs(y-11)//2
        bits=0
        for x in range(32):
            if (y in (1,22) and 12<=x<=19) or (1<y<22 and x in (edge,31-edge)):
                bits|=1<<(31-x)
        pixels.append(struct.pack('>I',bits))
    def string(s):
        b=s.encode('ascii')+b'\0'
        return struct.pack('>I',len(b))+b
    types=[f'SLAVE=Hexagon-{slots}.slave','PRELOAD','NOWRITECACHE']
    return bytes(header)+image+b''.join(pixels)+string('WHDLoad')+struct.pack('>I',4*(len(types)+1))+b''.join(string(s) for s in types)
