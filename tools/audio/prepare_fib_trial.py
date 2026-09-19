#!/usr/bin/env python3
"""Make an eight-second continuous-codec fixture from the accepted preview."""
from scipy.io import wavfile
import numpy as np
from compare_fib_encoders import load_encoder, encode
from codec_experiment import ROOT, fib_decode

rate, audio = wavfile.read(ROOT/'scratchpad/audio/courtesy/rate_test/12000/192k_fibonacci.wav')
assert rate == 12000 and audio.dtype == np.int16 and audio.ndim == 1
# Full blocks only. Re-encoding this already quantized preview isolates transport;
# this fixture is not a new quality candidate or a replacement dictionary.
samples = ((8*rate)//512)*512
q = (audio[:samples]//256).astype(np.int8)
data = encode(q, load_encoder())
assert len(data) == 12+(samples//512)*257
(ROOT/'out/fib_trial.fib').write_bytes(data)
(ROOT/'out/fib_trial.payload').write_bytes(data[12:])
wavfile.write(ROOT/'out/fib_trial_reference.wav', rate, fib_decode(data).astype(np.int16)*256)
print(samples, 'samples;', len(data)-12, 'payload bytes')
