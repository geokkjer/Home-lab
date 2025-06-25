<template>
  <aside class="fixed inset-y-0 left-0 z-50 w-64 bg-gray-900 overflow-y-auto">
    <div class="flex flex-col h-full">
      <!-- Sidebar header spacer -->
      <div class="h-16 flex items-center px-4">
        <!-- This matches the header height -->
      </div>

      <!-- Navigation -->
      <nav class="flex-1 px-4 pb-4 space-y-2">
        <div class="space-y-1">
          <button
            v-for="item in navigationItems"
            :key="item.id"
            type="button"
            :class="[
              'group flex items-center w-full px-2 py-2 text-sm font-medium rounded-md transition-colors',
              activeSection === item.id
                ? 'bg-gray-800 text-white'
                : 'text-gray-300 hover:bg-gray-700 hover:text-white'
            ]"
            @click="$emit('section-change', item.id)"
          >
            <component 
              :is="item.icon" 
              :class="[
                'mr-3 h-5 w-5 flex-shrink-0',
                activeSection === item.id ? 'text-white' : 'text-gray-400 group-hover:text-white'
              ]"
            />
            {{ item.name }}
            
            <!-- Badge for notifications -->
            <span 
              v-if="item.badge"
              :class="[
                'ml-auto inline-block py-0.5 px-2 text-xs rounded-full',
                item.badgeColor || 'bg-red-100 text-red-800'
              ]"
            >
              {{ item.badge }}
            </span>
          </button>
        </div>

        <!-- Section divider -->
        <div class="border-t border-gray-700 pt-4 mt-4">
          <h3 class="px-2 text-xs font-semibold text-gray-400 uppercase tracking-wider">
            Quick Links
          </h3>
          <div class="mt-2 space-y-1">
            <a
              v-for="link in quickLinks"
              :key="link.name"
              :href="link.href"
              target="_blank"
              rel="noopener noreferrer"
              class="group flex items-center px-2 py-2 text-sm font-medium rounded-md text-gray-300 hover:bg-gray-700 hover:text-white transition-colors"
            >
              <component 
                :is="link.icon" 
                class="mr-3 h-5 w-5 flex-shrink-0 text-gray-400 group-hover:text-white"
              />
              {{ link.name }}
              <svg class="ml-auto h-4 w-4 text-gray-400 group-hover:text-white" fill="currentColor" viewBox="0 0 20 20">
                <path fill-rule="evenodd" d="M11 3a1 1 0 100 2h2.586l-6.293 6.293a1 1 0 101.414 1.414L15 6.414V9a1 1 0 102 0V4a1 1 0 00-1-1h-5z" clip-rule="evenodd" />
                <path fill-rule="evenodd" d="M5 5a2 2 0 00-2 2v8a2 2 0 002 2h8a2 2 0 002-2v-3a1 1 0 10-2 0v3H5V7h3a1 1 0 000-2H5z" clip-rule="evenodd" />
              </svg>
            </a>
          </div>
        </div>
      </nav>

      <!-- Footer -->
      <div class="flex-shrink-0 px-4 py-4 border-t border-gray-700">
        <p class="text-xs text-gray-400">
          Last updated: {{ formatTime(new Date()) }}
        </p>
      </div>
    </div>
  </aside>
</template>

<script setup lang="ts">
import { h } from 'vue'

interface Props {
  activeSection: string
}

interface Emits {
  (e: 'section-change', section: string): void
}

defineProps<Props>()
defineEmits<Emits>()

// Simple icon components using h() function
const DashboardIcon = () => h('svg', { 
  fill: 'none', 
  viewBox: '0 0 24 24', 
  stroke: 'currentColor' 
}, [
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2H5a2 2 0 00-2-2z'
  }),
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M8 5v4M16 5v4'
  })
])

const ServerIcon = () => h('svg', { 
  fill: 'none', 
  viewBox: '0 0 24 24', 
  stroke: 'currentColor' 
}, [
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M5 12h14M5 12a2 2 0 01-2-2V6a2 2 0 012-2h14a2 2 0 012 2v4a2 2 0 01-2 2M5 12a2 2 0 00-2 2v4a2 2 0 002 2h14a2 2 0 002-2v-4a2 2 0 00-2-2m-2-4h.01M17 16h.01'
  })
])

const ChartIcon = () => h('svg', { 
  fill: 'none', 
  viewBox: '0 0 24 24', 
  stroke: 'currentColor' 
}, [
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z'
  })
])

const SettingsIcon = () => h('svg', { 
  fill: 'none', 
  viewBox: '0 0 24 24', 
  stroke: 'currentColor' 
}, [
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z'
  }),
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M15 12a3 3 0 11-6 0 3 3 0 016 0z'
  })
])

const ExternalLinkIcon = () => h('svg', { 
  fill: 'none', 
  viewBox: '0 0 24 24', 
  stroke: 'currentColor' 
}, [
  h('path', { 
    'stroke-linecap': 'round', 
    'stroke-linejoin': 'round', 
    'stroke-width': '2',
    d: 'M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14'
  })
])

const navigationItems = [
  {
    id: 'overview',
    name: 'Overview',
    icon: DashboardIcon
  },
  {
    id: 'services',
    name: 'Services',
    icon: ServerIcon,
    badge: '2',
    badgeColor: 'bg-red-100 text-red-800'
  },
  {
    id: 'monitoring',
    name: 'Monitoring',
    icon: ChartIcon
  },
  {
    id: 'settings',
    name: 'Settings',
    icon: SettingsIcon
  }
]

const quickLinks = [
  {
    name: 'Netdata',
    href: 'http://localhost:19999',
    icon: ExternalLinkIcon
  },
  {
    name: 'Ollama',
    href: 'http://localhost:11434',
    icon: ExternalLinkIcon
  }
]

const formatTime = (date: Date) => {
  return date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
}
</script>
