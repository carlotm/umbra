<template>
<section class="panel settings">
    <h3 class="panel__head">Settings</h3>
    <div class="panel__fieldset">
        <div class="panel__field panel__field--radio">
            <label class="panel__label">Shape</label>
            <input type="radio" value="square" id="id_square" v-model="shape">
            <label for="id_square" class="panel__radio-label">Square</label>
            <input type="radio" value="circle" id="id_circle" v-model="shape">
            <label for="id_circle" class="panel__radio-label">Circle</label>
        </div>
        <div class="panel__field">
            <label class="panel__label">Size</label>
            <input type="range" min="1" max="100" v-model="size">
            <span>{{ size }}px</span>
        </div>
        <div class="panel__field">
            <label class="panel__label">Color</label>
            <input type="color" v-model="color">
        </div>
    </div>
</section>
</template>

<script>
export default {
    name: 'Settings',
    computed: {
        size: {
            get() {
                return this.$store.state.settings.size;
            },
            set(value) {
                this.$store.dispatch('setSize', value);
            }
        },
        shape: {
            get() {
                return this.$store.state.settings.shape;
            },
            set(value) {
                this.$store.dispatch('setShape', value);
            }
        },
        color: {
            get() {
                return this.$store.state.settings.color;
            },
            set(value) {
                this.$store.dispatch('setColor', value);
            }
        }
    },
}
</script>
