import React, { useState } from 'react'
import { Box, Text, Button, Center } from '@chakra-ui/react'
import { AddIcon } from '@chakra-ui/icons'
import { ReactFlowProvider } from '@xyflow/react'
import { useWebSocket } from '../hooks/useWebSocket'
import { useBoardStore } from '../store/boardStore'
import { BoardCanvas } from '../components/BoardCanvas'
import { Toolbar } from '../components/Toolbar'
import { NodePalette } from '../components/NodePalette'

export default function BoardPage() {
  const { sendCommand, connectionState, sendFailed } = useWebSocket()
  const { currentBoardId, boards } = useBoardStore()
  const [showPalette, setShowPalette] = useState(false)

  return (
    <Box w="100vw" h="100vh" position="relative" overflow="hidden">
      <Toolbar
        sendCommand={sendCommand}
        connectionState={connectionState}
        sendFailed={sendFailed}
        showPalette={showPalette}
        onTogglePalette={() => setShowPalette((v) => !v)}
      />

      <Box position="absolute" top="48px" left={0} right={0} bottom={0}>
        {currentBoardId ? (
          <ReactFlowProvider>
            <BoardCanvas sendCommand={sendCommand} />
            <NodePalette
              isOpen={showPalette}
              onClose={() => setShowPalette(false)}
              sendCommand={sendCommand}
            />
          </ReactFlowProvider>
        ) : (
          <Center h="100%" flexDirection="column" gap={4}>
            <Text color="gray.400" fontSize="lg">
              {boards.length > 0
                ? 'Select a board from the toolbar'
                : 'Create your first board to get started'}
            </Text>
            {boards.length === 0 && (
              <Button
                leftIcon={<AddIcon />}
                colorScheme="blue"
                size="sm"
                onClick={() => sendCommand('create-board', { title: 'My First Board' })}
              >
                Create Board
              </Button>
            )}
          </Center>
        )}
      </Box>
    </Box>
  )
}
