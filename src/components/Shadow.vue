<template>
<section class="panel settings" v-if="currentShadow">
    <h3 class="panel__head">Shadow {{ currentShadow.pk }}</h3>
    <div class="panel__fieldset">
        <div class="panel__field">
            <label for="hoff" class="panel__label">Horizontal offset</label>
            <input type="range" id="hoff" min="-300" max="300" v-model="hoff">
            <span>{{ hoff }}px</span>
        </div>
        <div class="panel__field">
            <label for="voff" class="panel__label">Vertical offset</label>
            <input type="range" id="voff" min="-300" max="300" v-model="voff">
            <span>{{ voff }}px</span>
        </div>
        <div class="panel__field">
            <label for="blur" class="panel__label">Blur</label>
            <input type="range" id="blur" min="0" max="100" v-model="blur">
            <span>{{ blur }}px</span>
        </div>
        <div class="panel__field">
            <label for="spread" class="panel__label">Spread</label>
            <input type="range" id="spread" min="0" max="300" v-model="spread">
            <span>{{ spread }}px</span>
        </div>
        <div class="panel__field">
            <label for="curcolor" class="panel__label">Color</label>
            <input id="curcolor" type="color" v-model="curColor">
        </div>
        <div class="panel__field panel__field--spaced">
            <button class="btn btn--small btn--danger"
                @click="removeCurrentShadow()">
                Remove Shadow
            </button>
        </div>
    </div>
</section>
</template>

<script>
import { mapGetters } from 'vuex';

export default {
    name: 'Shadow',
    computed: {
        ...mapGetters([
            'currentShadow',
        ]),
        hoff: {
            get() {
                return this.$store.state.current.hoff;
            },
            set(value) {
                this.$store.dispatch('setHoff', value);
            }
        },
        voff: {
            get() {
                return this.$store.state.current.voff;
            },
            set(value) {
                this.$store.dispatch('setVoff', value);
            }
        },
        blur: {
            get() {
                return this.$store.state.current.blur;
            },
            set(value) {
                this.$store.dispatch('setBlur', value);
            }
        },
        spread: {
            get() {
                return this.$store.state.current.spread;
            },
            set(value) {
                this.$store.dispatch('setSpread', value);
            }
        },
        curColor: {
            get() {
                return this.$store.state.current.color;
            },
            set(value) {
                this.$store.dispatch('setCurColor', value);
            }
        },
    },
    methods: {
        removeCurrentShadow() {
            this.$store.dispatch('removeCurrentShadow');
        },
    }
}
</script>
