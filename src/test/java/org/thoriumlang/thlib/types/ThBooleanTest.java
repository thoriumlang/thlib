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

class ThBooleanTest {
    @Test
    void lazy() {
        Assertions.assertThat(ThBoolean.FALSE.lazy())
                .isEqualTo(ThLazyBoolean.FALSE);
        Assertions.assertThat(ThBoolean.TRUE.lazy())
                .isEqualTo(ThLazyBoolean.TRUE);
    }

    @Test
    void not() {
        Assertions.assertThat(ThBoolean.FALSE.not())
                .isEqualTo(ThBoolean.TRUE);
        Assertions.assertThat(ThBoolean.TRUE.not())
                .isEqualTo(ThBoolean.FALSE);
    }

    @Test
    void and() {
        Assertions.assertThat(ThBoolean.FALSE.and(ThBoolean.TRUE))
                .isEqualTo(ThBoolean.FALSE);
        Assertions.assertThat(ThBoolean.FALSE.and(ThBoolean.FALSE))
                .isEqualTo(ThBoolean.FALSE);
        Assertions.assertThat(ThBoolean.TRUE.and(ThBoolean.TRUE))
                .isEqualTo(ThBoolean.TRUE);
        Assertions.assertThat(ThBoolean.TRUE.and(ThBoolean.FALSE))
                .isEqualTo(ThBoolean.FALSE);
    }

    @Test
    void or() {
        Assertions.assertThat(ThBoolean.FALSE.or(ThBoolean.TRUE))
                .isEqualTo(ThBoolean.TRUE);
        Assertions.assertThat(ThBoolean.FALSE.or(ThBoolean.FALSE))
                .isEqualTo(ThBoolean.FALSE);
        Assertions.assertThat(ThBoolean.TRUE.or(ThBoolean.TRUE))
                .isEqualTo(ThBoolean.TRUE);
        Assertions.assertThat(ThBoolean.TRUE.or(ThBoolean.FALSE))
                .isEqualTo(ThBoolean.TRUE);
    }

    @Test
    void then() {
        Assertions.assertThat(ThBoolean.TRUE.then(() -> 1))
                .hasValue(1);
        Assertions.assertThat(ThBoolean.FALSE.then(() -> 1))
                .isNotPresent();
    }

    @Test
    void choose() {
        Assertions.assertThat(ThBoolean.TRUE.choose(() -> 1, () -> 2))
                .isEqualTo(1);
        Assertions.assertThat(ThBoolean.FALSE.choose(() -> 1, () -> 2))
                .isEqualTo(2);
    }
}
