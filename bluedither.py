import jax.numpy as jnp
import numpy as np
import jax
import matplotlib.pyplot as plt

# See https://bartwronski.com/2020/04/26/optimizing-blue-noise-dithering-backpropogation-through-fourier-transform-and-sorting
# esp Google Collab link

def spectrum(m):
    # Put the constant term in the centre
    # TODO: Is should this be a power spectrum?
    return jnp.abs(jmp.fft.fftshift(jnp.fft.fft2(m)))

def blue_noise_loss(m, SIZE):
    # Squared deviation from low frequency in the centre
    CUTOFF_FREQ = 0.7
    low_freq_weights = jnp.maximum(
        CUTOFF_FREQ
        - jnp.outer(jnp.square(np.linspace(1, -1, SIZE)), jnp.ones(SIZE))
        - jnp.outer(np.ones(SIZE), jnp.square(jnp.linspace(1, -1, SIZE))),
        0.0
    ) / CUTOFF_FREQ
    return jnp.sum(low_freq_weights * low_freq_weights * jnp.square(spectrum(m))) / (SIZE * SIZE)

def histogram_loss(m, SIZE):
    # Squared difference of sorted values as compared to the reference distribution
    reference_range = jnp.linspace(-1, 1, SIZE * SIZE)
    return jnp.sum(jnp.square(jnp.sort(m.flatten()) - reference_range))

def spectrum_uniformity_loss(m, SIZE):
    s = spectrum(m)
    laplacian = (-4.0 * s + jnp.roll(s, 1, axis=1) + jnp.roll(s, -1, axis=1) + jnp.roll(s, -1, axis=0)) / 5.0
    g_x = (s - jnp.roll(s, 1, axis=0)) / 2.0
    g_y = (s - jnp.roll(s, 1, axis=1)) / 2.0
    return (jnp.sum(jnp.square(laplacian)) + jnp.sum(jnp.square(g_x)) + jnp.sum(jnp.square(g_y))) / (SIZE * SIZE)

def full_loss(m, loss_component_1=1.0, loss_component_2=1.0, loss_component_3=1.0):
    spectral_loss = blue_noise_loss(m, SIZE) * loss_component_3
    range_loss = histogram_loss(m, SIZE) * loss_component_2
    uniformity_loss = spectrum_uniformity_loss(m, SIZE) * loss_component_1
    return 0.01 * uniformity_loss + 2.2 * range_loss + 0.1 * spectral_loss

grad = jax.jit(jax.grad(full_loss))
loss = jax.jit(full_loss)
%%
SIZE = 32
noise_mat = jnp.array((np.random.rand(SIZE, SIZE) - 0.5) * 2.0, dtype='float32')
noise_orig = noise_mat.copy()

def plot_results(m, SIZE):
    plt.rcParams['figure.figsize'] = 4, 4
    plt.matshow(m)
    plt.show()
    plt.hist(m.flatten(), bins=SIZE*SIZE)
    plt.show()
    plt.matshow(jnp.abs(jnp.fft.fftshift(jnp.fft.fft2(m))))
    plt.show()

plot_results(noise_mat, SIZE)
print(loss(noise_mat, 1.0, 0.0, 0.0), loss(noise_mat, 0.0, 1.0, 0.0), loss(noise_mat, 0.0, 0.0, 1.0), loss(noise_mat, 1.0, 1.0, 1.0))

N_ITER = 100000
for i in range(N_ITER):
    # Randomize the loss components relative weights and stochasticize it
    grads = grad(noise_mat, np.random.rand(), np.random.rand(), np.random.rand())
    noise_mat = noise_mat - 0.7 * grads
    # Once every 5k iterations, print the loss for verification
    if i % 5000 == 0:
        print(loss(noise_mat, 1.0, 0.0, 0.0), loss(noise_mat, 0.0, 1.0, 0.0), loss(noise_mat, 0.0, 0.0, 1.0), loss(noise_mat, 1.0, 1.0, 1.0), i / N_ITER * 100.0)
print(loss(noise_mat, 1.0, 0.0, 0.0), loss(noise_mat, 0.0, 1.0, 0.0), loss(noise_mat, 0.0, 0.0, 1.0), loss(noise_mat, 1.0, 1.0, 1.0))
# Fine tune the histogram
for i in range(10):
    grads = grad(noise_mat, 0.0, np.random.rand(), 0.0)
    noise_mat = noise_mat - 0.1 * grads
plot_results(noise_mat, SIZE)

# At this point things diverge.
# 1 bitplane dither - split into 1/3 and 2/3
# 2 bitplane dither - split into 1/15, 2/15, 4/15, 8/15

# Start with 50% dither. Optimise for blue noise using gradient descent.
# Work out which 25% pixels are most blue noisy.
# Repeat for 12.5%
# Repeat for 6.25%.
