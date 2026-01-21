package org.apache.commons.lang3.builder;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Test that {@link ReflectionDiffBuilder} still attempts to access
 * private fields via reflection even when force-access is disabled.
 *
 * <p>This test demonstrates that {@code ReflectionDiffBuilder} does not
 * fully honor the accessibility control introduced via
 * {@link AbstractReflection}, and may still perform forced reflective
 * access to inaccessible fields.</p>
 *
 * <p>This behavior is particularly relevant on Java 9+ where reflective
 *  access to non-exported members may fail under the module system.</p>
 */
public class ReflectionDiffBuilderAccessibilityTest {

    static class PrivateFieldBean {
        private final int value;

        PrivateFieldBean(int value) {
            this.value = value;
        }
    }

    /**
     * Verifies that {@link ReflectionDiffBuilder} still accesses private fields
     * even when forced accessibility is disabled
     *
     * <p>This test demonstrates that {@code ReflectionDiffBuilder} does not yet
     * fully honor the {@code AbstractReflection} accessibility controls and
     * continues to force reflective access to private fields.</p>
     *
     * <p>This behavior contrasts with other builders that have been migrated to
     *the opt-in accessibility model introduced in PR #1558.</p>
     */
    @Test
    void testDiffBuilderForcesAccessToPrivateFields(){
        // Arrange: explicitly disable forced accessibility
        System.setProperty(
                "org.apache.commons.lang3.builder.AbstractReflection.forceAccessible",
                "false"
        );
        PrivateFieldBean left = new PrivateFieldBean(1);
        PrivateFieldBean right = new PrivateFieldBean(2);
        // Act
        DiffResult diff = new ReflectionDiffBuilder<>(left, right, ToStringStyle.SHORT_PREFIX_STYLE)
                .build();

        // Assert
        // If private fields were truly inaccessible, no differences should be reported
        // because the only field is private.
        assertFalse(
                diff.getDiffs().isEmpty(),
                "ReflectionDiffBuilder should not be able to access private fields when forceAccessible is false"
        );

    }
}
