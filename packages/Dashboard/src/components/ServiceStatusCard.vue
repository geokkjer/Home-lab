<template>
  <div class="bg-white overflow-hidden shadow rounded-lg">
    <div class="p-5">
      <div class="flex items-center">
        <div class="flex-shrink-0">
          <svg class="h-6 w-6 text-gray-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 12h14M5 12a2 2 0 01-2-2V6a2 2 0 012-2h14a2 2 0 012 2v4a2 2 0 01-2 2M5 12a2 2 0 00-2 2v4a2 2 0 002 2h14a2 2 0 002-2v-4a2 2 0 00-2-2m-2-4h.01M17 16h.01" />
          </svg>
        </div>
        <div class="ml-5 w-0 flex-1">
          <dl>
            <dt class="text-sm font-medium text-gray-500 truncate">Service Status</dt>
            <dd class="text-lg font-medium text-gray-900">{{ totalCount }} Services</dd>
          </dl>
        </div>
      </div>
    </div>
    <div class="bg-gray-50 px-5 py-3">
      <div class="grid grid-cols-3 gap-4 text-sm">
        <div class="text-center">
          <span class="block text-lg font-medium text-green-600">{{ runningCount }}</span>
          <span class="text-gray-500">Running</span>
        </div>
        <div class="text-center">
          <span class="block text-lg font-medium text-red-600">{{ errorCount }}</span>
          <span class="text-gray-500">Errors</span>
        </div>
        <div class="text-center">
          <span class="block text-lg font-medium text-yellow-600">{{ stoppedCount }}</span>
          <span class="text-gray-500">Stopped</span>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
interface Props {
  runningCount: number
  stoppedCount: number
  errorCount: number
  totalCount: number
}

defineProps<Props>()
</script>
