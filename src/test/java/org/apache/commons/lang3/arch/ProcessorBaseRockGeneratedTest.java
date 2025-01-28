package org.apache.commons.lang3.arch;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ProcessorBaseRockGeneratedTest {

    //BaseRock generated method id: ${getArchTest}, hash: EB4201A1F076E9676DD33BB456A53990
    @Test()
    void getArchTest() {
        //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.AARCH_64);
        
        //Act Statement(s)
        Processor.Arch result = target.getArch();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Processor.Arch.BIT_32)));
    }

    //BaseRock generated method id: ${getTypeTest}, hash: 7533DF9D54FF4FF172BB759C43BB0B77
    @Test()
    void getTypeTest() {
        //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.AARCH_64);
        
        //Act Statement(s)
        Processor.Type result = target.getType();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Processor.Type.AARCH_64)));
    }

    //BaseRock generated method id: ${is32BitWhenArchBIT_32EqualsArch}, hash: 3ED63DD8D8AB62FC977F1720076E736D
    @Test()
    void is32BitWhenArchBIT_32EqualsArch() {
        /* Branches:
         * (Arch.BIT_32 == arch) : true
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.AARCH_64);
        
        //Act Statement(s)
        boolean result = target.is32Bit();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${is32BitWhenArchBIT_32NotEqualsArch}, hash: B60716A2CB6091E397BBEBFE62B62D91
    @Test()
    void is32BitWhenArchBIT_32NotEqualsArch() {
        /* Branches:
         * (Arch.BIT_32 == arch) : false
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_64, Processor.Type.AARCH_64);
        
        //Act Statement(s)
        boolean result = target.is32Bit();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${is64BitWhenArchBIT_64EqualsArch}, hash: 92D6C1D8F9EBB652D9AFD2A57071C263
    @Test()
    void is64BitWhenArchBIT_64EqualsArch() {
        /* Branches:
         * (Arch.BIT_64 == arch) : true
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_64, Processor.Type.AARCH_64);
        
        //Act Statement(s)
        boolean result = target.is64Bit();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${is64BitWhenArchBIT_64NotEqualsArch}, hash: 04B92476B0EFB2E5AD5EA3D726D7B108
    @Test()
    void is64BitWhenArchBIT_64NotEqualsArch() {
        /* Branches:
         * (Arch.BIT_64 == arch) : false
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.AARCH_64);
        
        //Act Statement(s)
        boolean result = target.is64Bit();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAarch64WhenTypeAARCH_64EqualsType}, hash: C586C552E69BD40F69657F4C918064E7
    @Test()
    void isAarch64WhenTypeAARCH_64EqualsType() {
        /* Branches:
         * (Type.AARCH_64 == type) : true
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.AARCH_64);
        
        //Act Statement(s)
        boolean result = target.isAarch64();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAarch64WhenTypeAARCH_64NotEqualsType}, hash: 23BCC87F602401F1F3F482BF77A2E0CF
    @Test()
    void isAarch64WhenTypeAARCH_64NotEqualsType() {
        /* Branches:
         * (Type.AARCH_64 == type) : false
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.UNKNOWN);
        
        //Act Statement(s)
        boolean result = target.isAarch64();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isIA64WhenTypeIA_64EqualsType}, hash: E334D4AB9543A9A93E8AFDB8B68E57B5
    @Test()
    void isIA64WhenTypeIA_64EqualsType() {
        /* Branches:
         * (Type.IA_64 == type) : true
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.IA_64);
        
        //Act Statement(s)
        boolean result = target.isIA64();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isIA64WhenTypeIA_64NotEqualsType}, hash: 535A1EA6B2D990EC5D1FA6C8A7A3E9B5
    @Test()
    void isIA64WhenTypeIA_64NotEqualsType() {
        /* Branches:
         * (Type.IA_64 == type) : false
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.UNKNOWN);
        
        //Act Statement(s)
        boolean result = target.isIA64();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isPPCWhenTypePPCEqualsType}, hash: 4C88D097E1DDEA15A6DBC1217072F135
    @Test()
    void isPPCWhenTypePPCEqualsType() {
        /* Branches:
         * (Type.PPC == type) : true
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.PPC);
        
        //Act Statement(s)
        boolean result = target.isPPC();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isPPCWhenTypePPCNotEqualsType}, hash: 34D0DF3607A62E5C080671D7F663E054
    @Test()
    void isPPCWhenTypePPCNotEqualsType() {
        /* Branches:
         * (Type.PPC == type) : false
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.UNKNOWN);
        
        //Act Statement(s)
        boolean result = target.isPPC();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isRISCVWhenTypeRISC_VEqualsType}, hash: 8B72C873431B511E55A2628E7FA93310
    @Test()
    void isRISCVWhenTypeRISC_VEqualsType() {
        /* Branches:
         * (Type.RISC_V == type) : true
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.RISC_V);
        
        //Act Statement(s)
        boolean result = target.isRISCV();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isRISCVWhenTypeRISC_VNotEqualsType}, hash: 1EC98FC8C974F13D96BF430D9CCE93D8
    @Test()
    void isRISCVWhenTypeRISC_VNotEqualsType() {
        /* Branches:
         * (Type.RISC_V == type) : false
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.UNKNOWN);
        
        //Act Statement(s)
        boolean result = target.isRISCV();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isX86WhenTypeX86EqualsType}, hash: 4D082A97B67E0B1447BC98FA95AC7312
    @Test()
    void isX86WhenTypeX86EqualsType() {
        /* Branches:
         * (Type.X86 == type) : true
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.X86);
        
        //Act Statement(s)
        boolean result = target.isX86();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isX86WhenTypeX86NotEqualsType}, hash: 8A2B0B18D1D041EED6AA772A1B1BC43D
    @Test()
    void isX86WhenTypeX86NotEqualsType() {
        /* Branches:
         * (Type.X86 == type) : false
         */
         //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.BIT_32, Processor.Type.UNKNOWN);
        
        //Act Statement(s)
        boolean result = target.isX86();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toStringTest}, hash: 74CBA6E890C153E4FA43AA6D7FE16682
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        Processor target = new Processor(Processor.Arch.UNKNOWN, Processor.Type.PPC);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("PPC Unknown")));
    }
}
