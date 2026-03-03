import React, { memo } from 'react'
import { NodeProps } from '@xyflow/react'
import { Box, Text } from '@chakra-ui/react'

interface BoardGroupData {
  label: string
  color?: string
}

const BoardGroupNode = ({ data }: NodeProps) => {
  const { label, color } = data as unknown as BoardGroupData

  return (
    <Box
      w="100%"
      h="100%"
      borderWidth="1px"
      borderStyle="dashed"
      borderColor="blue.200"
      borderRadius="md"
      bg={color || '#e8f0fe'}
      opacity={0.45}
      position="relative"
      pointerEvents="none"
      p={2}
    >
      <Text fontSize="xs" fontWeight="600" color="gray.700" noOfLines={1}>
        {label || 'Group'}
      </Text>
    </Box>
  )
}

export default memo(BoardGroupNode)
