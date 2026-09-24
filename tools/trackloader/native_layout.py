"""Fixed native disk ABI, shared by image generation and save-anchor validation."""
def reservations():
    return {'bootstrap': list(range(1661, 1705)),
            'persistent_a': list(range(1738, 1749)),
            'persistent_b': list(range(1749, 1760))}
