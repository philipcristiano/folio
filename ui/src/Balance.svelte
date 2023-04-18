<script lang="ts">
  import { onMount } from 'svelte';

  let balance = "";

  async function getBalance() {
    let response = await fetch("/api/holdings", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        balance = json.fiat_total;
    } else {
        message = json.message;
    };
  }
  onMount(() => {
      getBalance();
  });

</script>
<span class="text-md">Balance: ${ balance }</span>
