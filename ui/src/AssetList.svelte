<script lang="ts">

  import { onMount } from 'svelte';
  import Table from './Table.svelte';
  import TableElementTime from './TableElementTime.svelte';
  import Button from './Button.svelte';


  let message = "";
  let fields = [
    {name: "symbol", title: "Symbol"},
    {name: "external_id", title: "ID"},
    {name: "name", title: "Name"},
    {name: "last_price", title: "Price"},
    {name: "last_price_timestamp", title: "Updated", component: TableElementTime},
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
