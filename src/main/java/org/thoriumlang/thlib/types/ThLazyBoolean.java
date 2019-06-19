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

import java.util.Objects;
import java.util.Optional;

public class ThLazyBoolean implements ThLazy<ThBoolean> {
    public static final ThLazyBoolean TRUE = new ThLazyBoolean(() -> true);
    public static final ThLazyBoolean FALSE = new ThLazyBoolean(() -> false);

    private final ThLazy<Boolean> value;

    private ThLazyBoolean(ThLazy<Boolean> b) {
        this.value = b;
    }

    @Override
    public ThBoolean eval() {
        return value.eval() ? ThBoolean.TRUE : ThBoolean.FALSE;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ThLazyBoolean that = (ThLazyBoolean) o;
        return Objects.equals(value.eval(), that.value.eval());
    }

    @Override
    public int hashCode() {
        return Objects.hash(value.eval());
    }

    public ThLazyBoolean and(ThLazyBoolean other) {
        return new ThLazyBoolean(() -> value.eval() ? other.value.eval() : Boolean.FALSE);
    }

    public ThLazyBoolean or(ThLazyBoolean other) {
        return new ThLazyBoolean(() -> value.eval() ? Boolean.TRUE : other.value.eval());
    }

    public ThLazyBoolean not() {
        return new ThLazyBoolean(() -> value.eval() ? Boolean.FALSE : Boolean.TRUE);
    }

    public <T> ThLazy<Optional<T>> then(ThLazy<T> whenTrue) {
        return () -> this.value.eval() ? Optional.of(whenTrue.eval()) : Optional.empty();
    }

    public <T> ThLazy<T> choose(ThLazy<T> whenTrue, ThLazy<T> whenFalse) {
        return () -> value.eval() ? whenTrue.eval() : whenFalse.eval();
    }
}
