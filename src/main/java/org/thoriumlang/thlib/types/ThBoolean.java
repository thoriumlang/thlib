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

import java.util.Optional;

public class ThBoolean implements ThEager<ThLazyBoolean> {
    public static final ThBoolean TRUE = new ThBoolean(true);
    public static final ThBoolean FALSE = new ThBoolean(false);
    private final boolean value;

    private ThBoolean(boolean value) {
        this.value = value;
    }

    @Override
    public ThLazyBoolean lazy() {
        return value ? ThLazyBoolean.TRUE : ThLazyBoolean.FALSE;
    }

    public ThBoolean and(ThBoolean other) {
        return value ? other : FALSE;
    }

    public ThBoolean or(ThBoolean other) {
        return value ? TRUE : other;
    }

    public ThBoolean not() {
        return value ? FALSE : TRUE;
    }

    public <T> Optional<T> then(ThLazy<T> supplier) {
        return value ? Optional.of(supplier.eval()) : Optional.empty();
    }

    public <T> T choose(ThLazy<T> whenTrue, ThLazy<T> whenFalse) {
        return value ? whenTrue.eval() : whenFalse.eval();
    }
}
