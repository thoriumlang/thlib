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
package org.thoriumlang.thlib.pattern;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

class ThObjectPatternTest {
    @Test
    void match() {
        Assertions.assertThat(
                new ThObjectPattern<>("any", x -> x).matches("any")
        ).isTrue();

        Assertions.assertThat(
                new ThObjectPattern<>("other", x -> x).matches("any")
        ).isFalse();
    }

    @Test
    void eval() {
        Assertions.assertThat(
                new ThObjectPattern<>("any", x -> x).eval("any")
        ).hasValue("any");
    }
}
