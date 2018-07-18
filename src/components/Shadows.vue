<template>
<section class="shadows">
    <button class="btn" @click="addShadow()">Add Shadow</button>
    <div class="shadows__list">
        <div class="shadow" v-for="shadow in shadows" :key="shadow.pk">
            <input type="radio" name="shadows"
                :id="shadow.pk"
                :value="shadow.pk"
                @change="setCurrentShadow(shadow.pk)"
                :checked="getChecked(shadow)">
            <label :for="shadow.pk">{{ shadow.pk }}</label>
        </div>
    </div>
    <button class="btn btn--transparent" @click="loadExample()">Load example</button>
</section>
</template>

<script>
import { mapGetters } from 'vuex';

export default {
    name: 'Shadows',
    computed: {
        ...mapGetters([
            'shadows',
            'currentShadow',
            'boxshadowCssString',
        ]),
    },
    methods: {
        getChecked(s) {
            let cur = this.$store.getters.currentShadow;
            if (!cur)
                return false;
            return cur.pk === s.pk;
        },
        addShadow() {
            this.$store.dispatch('addShadow');
        },
        setCurrentShadow(pk) {
            this.$store.dispatch('setCurrentShadow', pk);
        },
        loadExample() {
            this.$store.dispatch('loadExample');
        },
    }
}
</script>
