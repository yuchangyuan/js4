/*
 * Copyright (C) 2006-2009 Vincent Hanquez <vincent@snarc.org>
 *               2016      Herbert Valerio Riedel <hvr@gnu.org>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef CRYPTOHASH_SHA512_H
#define CRYPTOHASH_SHA512_H

#include <stdint.h>
#include <stddef.h>

struct sha512_ctx
{
  uint64_t sz;
  uint64_t sz_hi;
  uint8_t  buf[128];
  uint64_t h[8];
};

/* keep this synchronised with 'digestSize'/'sizeCtx' in SHA512.hs */
#define SHA512_DIGEST_SIZE	64
#define SHA512_CTX_SIZE		208

void hs_cryptohash_sha512_init (struct sha512_ctx *ctx);
void hs_cryptohash_sha512_update (struct sha512_ctx *ctx, const uint8_t *data, size_t len);
void hs_cryptohash_sha512_finalize (struct sha512_ctx *ctx, uint8_t *out);

#endif
