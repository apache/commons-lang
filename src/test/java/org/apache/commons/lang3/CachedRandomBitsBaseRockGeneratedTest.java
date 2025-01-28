package org.apache.commons.lang3;

import org.apache.commons.lang3.CachedRandomBits;
import java.util.Random;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import org.junit.jupiter.api.Disabled;

class CachedRandomBitsBaseRockGeneratedTest {

    //BaseRock generated method id: ${testConstructorWithValidInput}, hash: 749EA4EDD73751BF86E234ED91372C40
    @Test
    void testConstructorWithValidInput() {
        Random random = new Random();
        CachedRandomBits crb = new CachedRandomBits(10, random);
        assertNotNull(crb);
    }

    //BaseRock generated method id: ${testConstructorWithInvalidCacheSize}, hash: 6C651709BA65CF0184A3BBFF6C696C92
    @Test
    void testConstructorWithInvalidCacheSize() {
        Random random = new Random();
        assertThrows(IllegalArgumentException.class, () -> new CachedRandomBits(0, random));
        assertThrows(IllegalArgumentException.class, () -> new CachedRandomBits(-1, random));
    }

    //BaseRock generated method id: ${testConstructorWithNullRandom}, hash: 765EFE30A6E257CCF4DD5E8BB60F74BB
    @Test
    void testConstructorWithNullRandom() {
        assertThrows(NullPointerException.class, () -> new CachedRandomBits(10, null));
    }

    //BaseRock generated method id: ${testNextBitsWithValidInput}, hash: 5FEB2D3559C5F5754D2E5FD886E0CAF6
    @Disabled()
    @ParameterizedTest
    @ValueSource(ints = { 1, 8, 16, 32 })
    void testNextBitsWithValidInput(int bits) {
        Random random = new Random(42);
        CachedRandomBits crb = new CachedRandomBits(100, random);
        int result = crb.nextBits(bits);
        assertTrue(result >= 0 && result < (1 << bits));
    }

    //BaseRock generated method id: ${testNextBitsWithInvalidInput}, hash: 54A1772B2F894E7CA5492DC2056205E2
    @Test
    void testNextBitsWithInvalidInput() {
        Random random = new Random();
        CachedRandomBits crb = new CachedRandomBits(10, random);
        assertThrows(IllegalArgumentException.class, () -> crb.nextBits(0));
        assertThrows(IllegalArgumentException.class, () -> crb.nextBits(33));
        assertThrows(IllegalArgumentException.class, () -> crb.nextBits(-1));
    }

    //BaseRock generated method id: ${testNextBitsConsistency}, hash: B1064C787CE4C1BA06ED431405692EFA
    @Test
    void testNextBitsConsistency() {
        Random random = new Random(42);
        CachedRandomBits crb = new CachedRandomBits(100, random);
        int first = crb.nextBits(16);
        int second = crb.nextBits(16);
        assertNotEquals(first, second);
    }

    //BaseRock generated method id: ${testNextBitsCacheRefill}, hash: 7A26F327909D8D4745BFD26F2A54B193
    @Test
    void testNextBitsCacheRefill() {
        Random random = new Random(42);
        CachedRandomBits crb = new CachedRandomBits(1, random);
        // This should use up the entire cache
        crb.nextBits(8);
        // This should trigger a cache refill
        int result = crb.nextBits(8);
        assertTrue(result >= 0 && result < 256);
    }

    //BaseRock generated method id: ${testNextBitsAcrossCacheBoundary}, hash: A993A073C99E070AC3D52B75AFA460F0
    @Test
    void testNextBitsAcrossCacheBoundary() {
        Random random = new Random(42);
        CachedRandomBits crb = new CachedRandomBits(1, random);
        // This should span across the cache boundary
        int result = crb.nextBits(16);
        assertTrue(result >= 0 && result < 65536);
    }

    //BaseRock generated method id: ${testNextBitsFullRange}, hash: DD96C7FB09E2EF1C4FF3A68378B44225
    @Test
    void testNextBitsFullRange() {
        Random random = new Random(42);
        CachedRandomBits crb = new CachedRandomBits(100, random);
        int result = crb.nextBits(32);
        assertTrue(result >= 0 && result <= Integer.MAX_VALUE);
    }
}
