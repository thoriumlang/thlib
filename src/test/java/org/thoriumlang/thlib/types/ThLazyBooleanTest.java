/*
 * Copyright 2019 Christophe Pollet
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.thoriumlang.thlib.types;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

class ThLazyBooleanTest {
    @Test
    void equals() {
        Assertions.assertThat(ThLazyBoolean.FALSE)
                .isEqualTo(ThLazyBoolean.FALSE);
        Assertions.assertThat(ThLazyBoolean.FALSE)
                .isNotEqualTo(ThLazyBoolean.TRUE);
        Assertions.assertThat(ThLazyBoolean.TRUE)
                .isEqualTo(ThLazyBoolean.TRUE);
        Assertions.assertThat(ThLazyBoolean.TRUE)
                .isNotEqualTo(ThLazyBoolean.FALSE);
    }

    @Test
    void eval() {
        Assertions.assertThat(ThLazyBoolean.FALSE.eval())
                .isFalse();
        Assertions.assertThat(ThLazyBoolean.TRUE.eval())
                .isTrue();
    }

    @Test
    void not() {
        Assertions.assertThat(ThLazyBoolean.FALSE.not())
                .isEqualTo(ThLazyBoolean.TRUE);
        Assertions.assertThat(ThLazyBoolean.TRUE.not())
                .isEqualTo(ThLazyBoolean.FALSE);
        Assertions.assertThat(ThLazyBoolean.FALSE.not().not())
                .isEqualTo(ThLazyBoolean.FALSE);
    }

    @Test
    void and() {
        Assertions.assertThat(ThLazyBoolean.FALSE.and(ThLazyBoolean.TRUE))
                .isEqualTo(ThLazyBoolean.FALSE);
        Assertions.assertThat(ThLazyBoolean.FALSE.and(ThLazyBoolean.FALSE))
                .isEqualTo(ThLazyBoolean.FALSE);
        Assertions.assertThat(ThLazyBoolean.TRUE.and(ThLazyBoolean.TRUE))
                .isEqualTo(ThLazyBoolean.TRUE);
        Assertions.assertThat(ThLazyBoolean.TRUE.and(ThLazyBoolean.FALSE))
                .isEqualTo(ThLazyBoolean.FALSE);
    }

    @Test
    void or() {
        Assertions.assertThat(ThLazyBoolean.FALSE.or(ThLazyBoolean.TRUE))
                .isEqualTo(ThLazyBoolean.TRUE);
        Assertions.assertThat(ThLazyBoolean.FALSE.or(ThLazyBoolean.FALSE))
                .isEqualTo(ThLazyBoolean.FALSE);
        Assertions.assertThat(ThLazyBoolean.TRUE.or(ThLazyBoolean.TRUE))
                .isEqualTo(ThLazyBoolean.TRUE);
        Assertions.assertThat(ThLazyBoolean.TRUE.or(ThLazyBoolean.FALSE))
                .isEqualTo(ThLazyBoolean.TRUE);
    }

    @Test
    void then() {
        Assertions.assertThat(ThLazyBoolean.TRUE.then(() -> 1).eval())
                .hasValue(1);
        Assertions.assertThat(ThLazyBoolean.FALSE.then(() -> 1).eval())
                .isNotPresent();
    }

    @Test
    void choose() {
        Assertions.assertThat(ThLazyBoolean.TRUE.choose(() -> 1, () -> 2).eval())
                .isEqualTo(1);
        Assertions.assertThat(ThLazyBoolean.FALSE.choose(() -> 1, () -> 2).eval())
                .isEqualTo(2);
    }
}
