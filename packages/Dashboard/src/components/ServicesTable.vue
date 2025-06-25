<template>
  <div class="overflow-hidden">
    <div class="flex justify-between items-center mb-4">
      <h4 class="text-sm font-medium text-gray-900">Service Status</h4>
      <button
        type="button"
        class="inline-flex items-center px-3 py-1 border border-gray-300 shadow-sm text-xs font-medium rounded text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
        @click="$emit('refresh')"
      >
        <svg class="h-3 w-3 mr-1" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15" />
        </svg>
        Refresh
      </button>
    </div>
    
    <div class="overflow-x-auto">
      <table class="min-w-full divide-y divide-gray-200">
        <thead class="bg-gray-50">
          <tr>
            <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
              Service
            </th>
            <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
              Status
            </th>
            <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
              Port
            </th>
            <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
              Actions
            </th>
          </tr>
        </thead>
        <tbody class="bg-white divide-y divide-gray-200">
          <tr v-for="service in services" :key="service.name">
            <td class="px-6 py-4 whitespace-nowrap">
              <div>
                <div class="text-sm font-medium text-gray-900">{{ service.name }}</div>
                <div v-if="service.description" class="text-sm text-gray-500">{{ service.description }}</div>
              </div>
            </td>
            <td class="px-6 py-4 whitespace-nowrap">
              <span :class="getStatusClass(service.status)" class="inline-flex px-2 py-1 text-xs font-semibold rounded-full">
                {{ service.status }}
              </span>
            </td>
            <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
              {{ service.port || '-' }}
            </td>
            <td class="px-6 py-4 whitespace-nowrap text-sm font-medium">
              <a
                v-if="service.url"
                :href="service.url"
                target="_blank"
                rel="noopener noreferrer"
                class="text-blue-600 hover:text-blue-900 mr-3"
              >
                Open
              </a>
              <button
                type="button"
                class="text-gray-600 hover:text-gray-900"
                @click="showServiceDetails(service)"
              >
                Details
              </button>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
</template>

<script setup lang="ts">
import type { ServiceStatus } from '@/stores/dashboard'

interface Props {
  services: readonly ServiceStatus[]
}

interface Emits {
  (e: 'refresh'): void
}

defineProps<Props>()
defineEmits<Emits>()

const getStatusClass = (status: string) => {
  switch (status) {
    case 'running':
      return 'bg-green-100 text-green-800'
    case 'stopped':
      return 'bg-yellow-100 text-yellow-800'
    case 'error':
      return 'bg-red-100 text-red-800'
    default:
      return 'bg-gray-100 text-gray-800'
  }
}

const showServiceDetails = (service: ServiceStatus) => {
  // Placeholder for service details functionality
  console.log('Show details for:', service.name)
}
</script>
