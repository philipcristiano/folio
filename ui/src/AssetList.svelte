<script lang="ts">

  import { onMount } from 'svelte';
  import Table from './Table.svelte';
  import Button from './Button.svelte';


  let message = "";
  let fields = [
    {name: "symbol", title: "Symbol"},
    {name: "external_id", title: "ID"},
    {name: "name", title: "Name"},
    ];
  let assets = [];

  async function getAssets() {
    let response = await fetch("/api/assets", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        assets = json.assets;
    } else {
        message = json.message;
    };
  }

  onMount(() => {
      getAssets();
  });

</script>

<div class="w-full">
  <div class="">

  TODO: FILTERS

  </div>

<Table fields={fields} data={assets} />

</div>
