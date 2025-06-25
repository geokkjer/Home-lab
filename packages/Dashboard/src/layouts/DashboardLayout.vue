<template>
  <div class="min-h-screen bg-gray-50">
    <DashboardHeader 
      v-if="user"
      :user="user"
      @logout="handleLogout"
    />
    
    <div class="flex">
      <DashboardSidebar 
        :active-section="activeSection"
        @section-change="handleSectionChange"
      />
      
      <main class="flex-1 p-6 ml-64">
        <div class="max-w-7xl mx-auto">
          <slot />
        </div>
      </main>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import DashboardHeader from '@/components/DashboardHeader.vue'
import DashboardSidebar from '@/components/DashboardSidebar.vue'
import type { User } from '@/stores/auth'

interface Props {
  user: User | null
}

interface Emits {
  (e: 'logout'): void
  (e: 'section-change', section: string): void
}

defineProps<Props>()
const emit = defineEmits<Emits>()

const activeSection = ref('overview')

const handleLogout = () => {
  emit('logout')
}

const handleSectionChange = (section: string) => {
  activeSection.value = section
  emit('section-change', section)
}
</script>
