<script lang="ts">
  import { onMount } from 'svelte';

  let balance = "";

  async function getBalance() {
    let response = await fetch("/api/balance", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        balance = json.fiat_value;
    } else {
        message = json.message;
    };
  }
  onMount(() => {
      getBalance();
  });

</script>
<p class="text-lg">Balance: ${ balance }</p>
