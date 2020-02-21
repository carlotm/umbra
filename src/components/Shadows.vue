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
    <button class="btn btn--transparent" @click="selectFile">
        Load
    </button>
    <input type="file" style="display: none; visibility: hidden;"
        accept=".json" @change="load" />
    <button class="btn btn--transparent" @click="save">
        Save
    </button>
    <button class="btn btn--transparent" @click="exportCSS">
        CSS Code
    </button>
    <button class="btn btn--transparent" @click="loadExample">
        Guybrush!
    </button>
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
        selectFile(e) {
            e.target.nextSibling.click();
        },
        load(e) {
            this.$store.dispatch('load', e.target.files);
        },
        save() {
            this.$store.dispatch('save');
        },
        exportCSS() {
            this.$store.dispatch('export');
        },
    }
}
</script>
