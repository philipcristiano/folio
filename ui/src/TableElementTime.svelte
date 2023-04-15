<script lang="ts">
  import Time from "svelte-time";

  export let value = "";

</script>
<Time relative timestamp="{value}"/>

