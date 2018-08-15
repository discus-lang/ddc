
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include "Runtime.h"
#include "runtime/Primitive.h"


// ------------------------------------------------------------------------------------------------
// Length of the chunk buffer in bytes.
#define CHUNK_LENGTH 64

struct sha256_state {
        // Current hash value.
        uint32_t *hash;

        // Pointer to the chunk buffer.
        uint8_t  *chunk;

        // Index of next byte to write into the chunk buffer.
        size_t   ixChunk;

        // Number of bytes pushed into the chunk so far.
        uint64_t lenMessage;
};


// ------------------------------------------------------------------------------------------------
// Round constants.
//   The first 32 bits of the fractional parts of the cube roots of the
//   first 64 primes 2..311.
static const uint32_t k[] = {
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};


static const uint32_t h_init[] = {
  0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 };


// ------------------------------------------------------------------------------------------------
static inline uint32_t right_rot(uint32_t value, unsigned int count)
{
        /* Defined behaviour in standard C for all count where 0 < count < 32,
         * which is what we need here. */
        return value >> count | value << (32 - count);
}


// ------------------------------------------------------------------------------------------------
// Allocate a new hasher state.
struct sha256_state* ddcPrimSha256Begin(Obj* unit)
{
        struct sha256_state *state
                = malloc(sizeof(struct sha256_state));

        // Allcoate and initialize the starting hash value.
        state->hash     = malloc(sizeof(uint32_t) * 8);
        for (int i = 0; i < 8; i++)
                state->hash[i] = h_init[i];

        // Allocate space for the current chunk.
        state->chunk    = malloc(sizeof(uint32_t) * 16);
        memset(state->chunk, 0x00, sizeof(uint32_t) * 8);

        state->ixChunk    = 0;
        state->lenMessage = 0;

        return state;
}


// ------------------------------------------------------------------------------------------------
// Push a single byte into the hashed stream.
void ddcPrimSha256PushWord8
        (struct sha256_state* state, uint8_t w8)
{
        // If we don't have space in the current chunk then compress
        // the existing data into the hash.
        if (state->ixChunk + 1 >= CHUNK_LENGTH)
                ddcPrimSha256ProcessChunk(state);

        // Add the new byte to the chunk.
        state->chunk[state->ixChunk++] = w8;
        state->lenMessage++;
}


static inline void ddcPrimSha256PushWord8_inline
        (struct sha256_state* state, uint8_t w8)
{
        // If we don't have space in the current chunk then compress
        // the existing data into the hash.
        if (state->ixChunk >= CHUNK_LENGTH)
                ddcPrimSha256ProcessChunk(state);

        // Add the new byte to the chunk.
        state->chunk[state->ixChunk++] = w8;
        state->lenMessage++;
}


// Push a Word64 byte into the hash stream,
// converting it to the required MSB order on the way.
void ddcPrimSha256PushWord64
        (struct sha256_state* state, uint64_t w64)
{
        ddcPrimSha256PushWord8_inline (state, (w64 >> 56) & 0x0ff);
        ddcPrimSha256PushWord8_inline (state, (w64 >> 48) & 0x0ff);
        ddcPrimSha256PushWord8_inline (state, (w64 >> 40) & 0x0ff);
        ddcPrimSha256PushWord8_inline (state, (w64 >> 32) & 0x0ff);
        ddcPrimSha256PushWord8_inline (state, (w64 >> 24) & 0x0ff);
        ddcPrimSha256PushWord8_inline (state, (w64 >> 16) & 0x0ff);
        ddcPrimSha256PushWord8_inline (state, (w64 >>  8) & 0x0ff);
        ddcPrimSha256PushWord8_inline (state,  w64        & 0x0ff);
}


// Push a text literal into the hash stream, in MSB order.
void ddcPrimSha256PushTextLit
        (struct sha256_state* state, Obj* obj)
{
        uint8_t* str    = ddcPrimTakeTextLit(obj);
        nat_t   len     = ddcPrimSizeOfTextLit(obj);

        for (nat_t i = 0; i < len; i++)
                ddcPrimSha256PushWord8_inline(state, str[i]);
}


// Push a vector of bytes into the hash stream, in MSB order.
void ddcPrimSha256PushVector8
        (struct sha256_state* state, Obj* obj)
{
        uint8_t* str    = ddcPrimVectorPayload8(obj);
        nat_t   len     = ddcPrimVectorLength(obj);

        for (nat_t i = 0; i < len; i++)
                ddcPrimSha256PushWord8_inline(state, str[i]);
}



// ------------------------------------------------------------------------------------------------
// Process a complete chunk and update the current hash.
void ddcPrimSha256ProcessChunk
        (struct sha256_state* state)
{
        // We can only hash complete chunks.
        assert (state->ixChunk == CHUNK_LENGTH);

        uint32_t a[8];
        uint32_t w[64];
        uint32_t *h  = state->hash;
        uint8_t  *c  = state->chunk;

        // Copy chunk into the first 16 words of the message schedule array.
        for (int i = 0; i < 16; i++)
        {       w[i]    = (uint32_t)c[0] << 24
                        | (uint32_t)c[1] << 16
                        | (uint32_t)c[2] << 8
                        | (uint32_t)c[3];
                c += 4;
        }

        // Extend the first 16 words of the chunk into the remaining 46 words
        // w[16..63] of the message schedule array.
        for (int i = 16; i < 64; i++)
        {       uint32_t s0 = right_rot(w[i - 15], 7) ^ right_rot(w[i - 15], 18) ^ (w[i - 15] >> 3);
                uint32_t s1 = right_rot(w[i - 2], 17) ^ right_rot(w[i -  2], 19) ^ (w[i -  2] >> 10);
                w[i] = w[i - 16] + s0 + w[i - 7] + s1;
        }

        // Initialize working variables to current hash value.
        for (int i = 0; i < 8; i++)
                a[i] = h[i];

        // Compression function main loop.
        for (int i = 0; i < 64; i++)
        {       uint32_t s1    = right_rot(a[4], 6) ^ right_rot(a[4], 11) ^ right_rot(a[4], 25);
                uint32_t ch    = (a[4] & a[5]) ^ (~a[4] & a[6]);
                uint32_t temp1 = a[7] + s1 + ch + k[i] + w[i];
                uint32_t s0    = right_rot(a[0], 2) ^ right_rot(a[0], 13) ^ right_rot(a[0], 22);
                uint32_t maj   = (a[0] & a[1]) ^ (a[0] & a[2]) ^ (a[1] & a[2]);
                uint32_t temp2 = s0 + maj;

                a[7] = a[6];
                a[6] = a[5];
                a[5] = a[4];
                a[4] = a[3] + temp1;
                a[3] = a[2];
                a[2] = a[1];
                a[1] = a[0];
                a[0] = temp1 + temp2;
        }

        // Add the compressed chunk to the current hash value.
        for (int i = 0; i < 8; i++)
                h[i] += a[i];

        // Set the number of used bytes in the chunk back to zero.
        state->ixChunk  = 0;
}


// ------------------------------------------------------------------------------------------------
// Eject the completed hash value and deallocate the state.
Obj* ddcPrimSha256Eject
        (struct sha256_state* state)
{
        uint32_t *h   = state->hash;
        Obj* oOutput  = ddcPrimVectorAlloc8(32);
        uint8_t* hash = ddcPrimVectorPayload8(oOutput);

        // Save the length of the user message.
        // When we add further padding we don't want that to count that
        // as part of the message.
        uint64_t lenUserMessage = state->lenMessage;

        // Append a single 1 bit.
        ddcPrimSha256PushWord8(state, 0x80);

        // Number of bytes of space remaining in the chunk.
        size_t lenSpace = CHUNK_LENGTH - state->ixChunk;

        // Work out the number of bytes of padding we need to add to bring
        // the data up to a multiple of 512 bits (64 bytes) with the size
        // field on the end.
        size_t lenPad;
        if (lenSpace >= 8)
                lenPad   = lenSpace - 8;
        else
                lenPad   = CHUNK_LENGTH - (8 - lenSpace);

        // Push the padding on to the end of the chunk.
        for (size_t i = 0; i < lenPad; i++)
                ddcPrimSha256PushWord8(state, 0);

        // Push the length field.
        ddcPrimSha256PushWord64(state, lenUserMessage * 8);

        // Process the final chunk.
        // After adding the padding above there should be a complete chunk
        // ready to be compressed.
        ddcPrimSha256ProcessChunk(state);

        // Extract the final hash value, big endian.
        int i, j;
        for (i = 0, j = 0; i < 8; i++)
        {       hash[j++] = (uint8_t) (h[i] >> 24);
                hash[j++] = (uint8_t) (h[i] >> 16);
                hash[j++] = (uint8_t) (h[i] >> 8);
                hash[j++] = (uint8_t)  h[i];
        }

        // Free space for the hasher state.
        free(state->hash);
        free(state->chunk);
        free(state);

        return oOutput;
}

