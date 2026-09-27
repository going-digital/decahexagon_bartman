"""Repack the flat native OFS volume in startup order, preserving its disk ABI."""
import struct
from check_adf_layout import inspect

def pack(data, expected, reservations):
    layout = inspect(data, expected, reservations)
    bitmap = struct.unpack_from('>I', data, 880 * 512 + 316)[0]
    fixed = {0, 1, 880, bitmap}
    for sectors in reservations.values():
        fixed.update(sectors)
    free = iter(n for n in range(2, 1760) if n not in fixed)
    mapping = {}
    # Put each extension immediately before the data it describes. The runtime
    # retains metadata separately, so it need not evict the track cache again.
    for name in ('game', 'courtesy', 'otis', 'focus', 'save_a', 'save_b'):
        file = layout['files'][name]
        for i, metadata in enumerate(file['metadata_sectors']):
            mapping[metadata] = next(free)
            for sector in file['data_sectors'][i * 72:(i + 1) * 72]:
                mapping[sector] = next(free)
    result = bytearray(len(data))
    for sector in fixed:
        result[sector * 512:(sector + 1) * 512] = data[sector * 512:(sector + 1) * 512]
    def write(sector, words, checksum=5):
        words[checksum] = 0
        words[checksum] = -sum(words) & 0xffffffff
        struct.pack_into('>128I', result, sector * 512, *words)
    for old, new in mapping.items():
        words = list(struct.unpack_from('>128I', data, old * 512))
        words[1] = new if words[0] != 8 else mapping[words[1]]
        words[4] = mapping.get(words[4], words[4])
        if words[0] != 8:
            for index in range(78 - words[2], 78):
                words[index] = mapping[words[index]]
            for index in (124, 126):
                words[index] = mapping.get(words[index], words[index])
        write(new, words)
    root = list(struct.unpack_from('>128I', data, 880 * 512))
    for index in range(6, 78):
        root[index] = mapping.get(root[index], root[index])
    write(880, root)
    bits = [0] * 128
    occupied = fixed | set(mapping.values())
    for sector in range(2, 1760):
        if sector not in occupied:
            bits[1 + (sector - 2) // 32] |= 1 << ((sector - 2) % 32)
    write(bitmap, bits, 0)
    inspect(result, expected, reservations)
    return result

def read_cost(data, layout):
    """Model the real reader's root/hash/extension order and one-track cache."""
    rows = {}
    for name in ('game', 'courtesy', 'otis', 'focus'):
        file = layout['files'][name]
        h = len(name)
        for c in name.upper(): h = (h * 13 + ord(c)) & 0x7ff
        sector = struct.unpack_from('>I', data, 880*512 + (6+h%72)*4)[0]
        sequence = [880]
        while sector != file['header']:
            sequence.append(sector)
            sector = struct.unpack_from('>I', data, sector*512+124*4)[0]
        for i, meta in enumerate(file['metadata_sectors']):
            sequence.append(meta)
            sequence.extend(file['data_sectors'][i*72:(i+1)*72])
        tracks = []
        for sector in sequence:
            if not tracks or tracks[-1] != sector//11: tracks.append(sector//11)
        rows[name] = dict(sector_requests=len(sequence), track_reads=len(tracks),
                         distinct_tracks=len(set(tracks)),
                         cylinder_travel=sum(abs(a//2-b//2) for a,b in zip(tracks,tracks[1:])))
    return rows
