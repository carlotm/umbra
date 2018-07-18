<template>
<section class="workspace">
    <Shadow/>
    <section class="canvas">
        <div class="item" :style="cssString"></div>
    </section>
    <Settings/>
</section>
</template>

<script>
import Shadow from './Shadow.vue';
import Settings from './Settings.vue';
import { mapGetters } from 'vuex';

export default {
    name: 'Item',
    components: {
        Shadow,
        Settings,
    },
    computed: {
        ...mapGetters([
            'cssString',
        ])
    }
}
</script>
