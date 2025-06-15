// VS Code Extension for Home Lab MCP Integration
// Run: npm init -y && npm install @types/vscode @types/node typescript

import * as vscode from 'vscode';
import { spawn, ChildProcess } from 'child_process';

interface MCPRequest {
    jsonrpc: string;
    id: number;
    method: string;
    params?: any;
}

interface MCPResponse {
    jsonrpc: string;
    id: number;
    result?: any;
    error?: any;
}

export class HomeLabMCPExtension {
    private mcpProcess: ChildProcess | null = null;
    private requestId = 0;
    private pendingRequests = new Map<number, (response: MCPResponse) => void>();
    private statusBarItem: vscode.StatusBarItem;

    constructor(private context: vscode.ExtensionContext) {
        this.statusBarItem = vscode.window.createStatusBarItem(
            vscode.StatusBarAlignment.Left, 
            100
        );
        this.statusBarItem.text = "$(server-environment) Home Lab: Disconnected";
        this.statusBarItem.show();
    }

    async activate() {
        // Register commands
        this.context.subscriptions.push(
            vscode.commands.registerCommand('homelab.connect', () => this.connect()),
            vscode.commands.registerCommand('homelab.disconnect', () => this.disconnect()),
            vscode.commands.registerCommand('homelab.deploy', (machine) => this.deployMachine(machine)),
            vscode.commands.registerCommand('homelab.status', () => this.showStatus()),
            vscode.commands.registerCommand('homelab.generateConfig', () => this.generateConfig()),
            vscode.commands.registerCommand('homelab.listTools', () => this.listAvailableTools()),
            vscode.commands.registerCommand('homelab.executeTool', () => this.executeToolInteractive())
        );

        // Start MCP server
        await this.connect();

        // Set up context for Copilot
        this.setupCopilotContext();
    }

    private async disconnect(): Promise<void> {
        if (this.mcpProcess) {
            this.mcpProcess.kill();
            this.mcpProcess = null;
        }
        this.statusBarItem.text = "$(server-environment) Home Lab: Disconnected";
        vscode.window.showInformationMessage('Disconnected from Home Lab MCP Server');
    }

    private async listAvailableTools(): Promise<void> {
        try {
            const tools = await this.sendMCPRequest('tools/list', {});
            
            const panel = vscode.window.createWebviewPanel(
                'homelabTools',
                'Home Lab Tools',
                vscode.ViewColumn.One,
                { enableScripts: true }
            );

            panel.webview.html = this.getToolsHTML(tools.tools);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to get tools: ${error}`);
        }
    }

    private async executeToolInteractive(): Promise<void> {
        try {
            const tools = await this.sendMCPRequest('tools/list', {});
            
            const toolName = await vscode.window.showQuickPick(
                tools.tools?.map((t: any) => ({
                    label: t.name,
                    description: t.description,
                    detail: `Parameters: ${Object.keys(t.inputSchema?.properties || {}).join(', ')}`
                })),
                { placeHolder: 'Select tool to execute' }
            );

            if (!toolName) return;

            const tool = tools.tools.find((t: any) => t.name === toolName.label);
            const args: any = {};

            // Collect parameters interactively
            if (tool.inputSchema?.properties) {
                for (const [paramName, paramSchema] of Object.entries(tool.inputSchema.properties)) {
                    const value = await vscode.window.showInputBox({
                        prompt: `Enter ${paramName}`,
                        placeHolder: (paramSchema as any).description || `Value for ${paramName}`
                    });
                    if (value !== undefined) {
                        args[paramName] = value;
                    }
                }
            }

            const result = await this.sendMCPRequest('tools/call', {
                name: tool.name,
                arguments: args
            });

            // Show result in output channel
            const output = vscode.window.createOutputChannel('Home Lab Tool Result');
            output.clear();
            output.appendLine(`Tool: ${tool.name}`);
            output.appendLine(`Arguments: ${JSON.stringify(args, null, 2)}`);
            output.appendLine('---');
            output.appendLine(JSON.stringify(result, null, 2));
            output.show();

        } catch (error) {
            vscode.window.showErrorMessage(`Failed to execute tool: ${error}`);
        }
    }

    private async connect(): Promise<void> {
        try {
            // Start Guile MCP server
            this.mcpProcess = spawn('guile', [
                '-L', vscode.workspace.rootPath + '/packages',
                '-c', '(use-modules (mcp server)) (run-mcp-server)'
            ], {
                stdio: ['pipe', 'pipe', 'pipe'],
                cwd: vscode.workspace.rootPath
            });

            this.mcpProcess.stdout?.on('data', (data) => {
                this.handleMCPResponse(data.toString());
            });

            this.mcpProcess.stderr?.on('data', (data) => {
                console.error('MCP Error:', data.toString());
            });

            // Initialize MCP session
            await this.sendMCPRequest('initialize', {
                protocolVersion: '2024-11-05',
                capabilities: {
                    tools: {},
                    resources: {}
                },
                clientInfo: {
                    name: 'vscode-homelab',
                    version: '0.1.0'
                }
            });

            this.statusBarItem.text = "$(server-environment) Home Lab: Connected";
            vscode.window.showInformationMessage('Connected to Home Lab MCP Server');

        } catch (error) {
            this.statusBarItem.text = "$(server-environment) Home Lab: Error";
            vscode.window.showErrorMessage(`Failed to connect to MCP server: ${error}`);
        }
    }

    private async sendMCPRequest(method: string, params?: any): Promise<any> {
        if (!this.mcpProcess?.stdin) {
            throw new Error('MCP server not connected');
        }

        const id = ++this.requestId;
        const request: MCPRequest = {
            jsonrpc: '2.0',
            id,
            method,
            params
        };

        return new Promise((resolve, reject) => {
            this.pendingRequests.set(id, (response: MCPResponse) => {
                if (response.error) {
                    reject(new Error(response.error.message));
                } else {
                    resolve(response.result);
                }
            });

            this.mcpProcess!.stdin!.write(JSON.stringify(request) + '\n');
            
            // Timeout after 30 seconds
            setTimeout(() => {
                if (this.pendingRequests.has(id)) {
                    this.pendingRequests.delete(id);
                    reject(new Error('Request timeout'));
                }
            }, 30000);
        });
    }

    private handleMCPResponse(data: string): void {
        try {
            const lines = data.trim().split('\n');
            for (const line of lines) {
                if (line.trim()) {
                    const response: MCPResponse = JSON.parse(line);
                    const handler = this.pendingRequests.get(response.id);
                    if (handler) {
                        this.pendingRequests.delete(response.id);
                        handler(response);
                    }
                }
            }
        } catch (error) {
            console.error('Failed to parse MCP response:', error);
        }
    }

    async deployMachine(machine?: string): Promise<void> {
        if (!machine) {
            const machines = await this.getMachines();
            machine = await vscode.window.showQuickPick(machines, {
                placeHolder: 'Select machine to deploy'
            });
        }

        if (!machine) return;

        const method = await vscode.window.showQuickPick(
            ['deploy-rs', 'hybrid-update', 'legacy'],
            { placeHolder: 'Select deployment method' }
        );

        if (!method) return;

        try {
            vscode.window.withProgress({
                location: vscode.ProgressLocation.Notification,
                title: `Deploying ${machine}...`,
                cancellable: false
            }, async (progress) => {
                const result = await this.sendMCPRequest('tools/call', {
                    name: 'deploy-machine',
                    arguments: { machine, method }
                });

                if (result.success) {
                    vscode.window.showInformationMessage(
                        `Successfully deployed ${machine} using ${method}`
                    );
                } else {
                    vscode.window.showErrorMessage(
                        `Deployment failed: ${result.error || 'Unknown error'}`
                    );
                }
            });
        } catch (error) {
            vscode.window.showErrorMessage(`Deployment error: ${error}`);
        }
    }

    async showStatus(): Promise<void> {
        try {
            const status = await this.sendMCPRequest('resources/read', {
                uri: 'homelab://status/all'
            });

            const panel = vscode.window.createWebviewPanel(
                'homelabStatus',
                'Home Lab Status',
                vscode.ViewColumn.One,
                { enableScripts: true }
            );

            panel.webview.html = this.getStatusHTML(status.content);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to get status: ${error}`);
        }
    }

    async generateConfig(): Promise<void> {
        const machineName = await vscode.window.showInputBox({
            prompt: 'Enter machine name',
            placeHolder: 'my-new-machine'
        });

        if (!machineName) return;

        const services = await vscode.window.showInputBox({
            prompt: 'Enter services (comma-separated)',
            placeHolder: 'nginx,postgresql,redis'
        });

        try {
            const result = await this.sendMCPRequest('tools/call', {
                name: 'generate-nix-config',
                arguments: {
                    'machine-name': machineName,
                    services: services ? services.split(',').map(s => s.trim()) : []
                }
            });

            const doc = await vscode.workspace.openTextDocument({
                content: result.content,
                language: 'nix'
            });

            await vscode.window.showTextDocument(doc);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to generate config: ${error}`);
        }
    }

    private async getMachines(): Promise<string[]> {
        try {
            const result = await this.sendMCPRequest('tools/call', {
                name: 'list-machines',
                arguments: {}
            });
            return result.machines || [];
        } catch (error) {
            console.error('Failed to get machines:', error);
            return [];
        }
    }

    private setupCopilotContext(): void {
        // Create a virtual document that provides context to Copilot
        const provider = new (class implements vscode.TextDocumentContentProvider {
            constructor(private extension: HomeLabMCPExtension) {}
            
            async provideTextDocumentContent(): Promise<string> {
                try {
                    const context = await this.extension.sendMCPRequest('resources/read', {
                        uri: 'homelab://context/copilot'
                    });
                    return context.content;
                } catch (error) {
                    return `# Home Lab Context\nError loading context: ${error}`;
                }
            }
        })(this);

        this.context.subscriptions.push(
            vscode.workspace.registerTextDocumentContentProvider(
                'homelab-context',
                provider
            )
        );

        // Register workspace context provider for Copilot
        this.registerCopilotWorkspaceProvider();

        // Open the context document to make it available to Copilot
        vscode.workspace.openTextDocument(vscode.Uri.parse('homelab-context://context')).then(doc => {
            // Keep it open but hidden for context
        });
    }

    private registerCopilotWorkspaceProvider(): void {
        // Enhanced Copilot integration using VS Code's context API
        const workspaceProvider = {
            provideWorkspaceContext: async () => {
                try {
                    // Get comprehensive home lab context
                    const [status, machines, services] = await Promise.all([
                        this.sendMCPRequest('resources/read', { uri: 'homelab://status/summary' }),
                        this.sendMCPRequest('tools/call', { name: 'list-machines', arguments: {} }),
                        this.sendMCPRequest('tools/call', { name: 'list-services', arguments: {} })
                    ]);

                    return {
                        name: 'Home Lab Infrastructure',
                        description: 'Current state and configuration of home lab environment',
                        content: `# Home Lab Infrastructure Context

## Available Machines
${machines.machines?.map((m: any) => `- ${m.name}: ${m.status} (${m.services?.join(', ') || 'no services'})`).join('\n') || 'No machines found'}

## Service Status
${services.services?.map((s: any) => `- ${s.name}: ${s.status} on ${s.machine}`).join('\n') || 'No services found'}

## Current Infrastructure State
${status.summary || 'Status unavailable'}

## Available Operations
- deploy-machine: Deploy configuration to a specific machine
- check-status: Get detailed status of machines and services  
- generate-config: Create new NixOS configurations
- manage-services: Start/stop/restart services
- backup-data: Backup service data and configurations

Use these MCP tools for infrastructure operations through the home lab extension.`
                    };
                } catch (error) {
                    return {
                        name: 'Home Lab Infrastructure',
                        description: 'Home lab context (error loading)',
                        content: `# Home Lab Infrastructure Context\n\nError loading context: ${error}\n\nTry connecting to MCP server first.`
                    };
                }
            }
        };

        // Register the workspace provider if the API is available
        if ((vscode as any).workspace.registerWorkspaceContextProvider) {
            this.context.subscriptions.push(
                (vscode as any).workspace.registerWorkspaceContextProvider(workspaceProvider)
            );
        }
    }

    private getStatusHTML(status: any): string {
        return `
        <!DOCTYPE html>
        <html>
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>Home Lab Status</title>
            <style>
                body { font-family: -apple-system, BlinkMacSystemFont, sans-serif; margin: 20px; }
                .machine { border: 1px solid #ccc; margin: 10px 0; padding: 15px; border-radius: 5px; }
                .online { border-left: 4px solid #28a745; }
                .offline { border-left: 4px solid #dc3545; }
                .service { margin: 5px 0; padding: 5px; background: #f8f9fa; border-radius: 3px; }
            </style>
        </head>
        <body>
            <h1>Home Lab Status</h1>
            <pre>${JSON.stringify(status, null, 2)}</pre>
        </body>
        </html>
        `;
    }

    private getToolsHTML(tools: any[]): string {
        const toolsHTML = tools?.map(tool => `
            <div class="tool">
                <h3>${tool.name}</h3>
                <p><strong>Description:</strong> ${tool.description || 'No description'}</p>
                <details>
                    <summary>Parameters</summary>
                    <pre>${JSON.stringify(tool.inputSchema?.properties || {}, null, 2)}</pre>
                </details>
            </div>
        `).join('') || '<p>No tools available</p>';

        return `
        <!DOCTYPE html>
        <html>
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>Home Lab Tools</title>
            <style>
                body { font-family: -apple-system, BlinkMacSystemFont, sans-serif; margin: 20px; }
                .tool { border: 1px solid #ddd; margin: 15px 0; padding: 15px; border-radius: 8px; }
                .tool h3 { margin-top: 0; color: #0066cc; }
                details { margin-top: 10px; }
                summary { cursor: pointer; font-weight: bold; }
                pre { background: #f5f5f5; padding: 10px; border-radius: 4px; overflow-x: auto; }
            </style>
        </head>
        <body>
            <h1>Available Home Lab Tools</h1>
            ${toolsHTML}
        </body>
        </html>
        `;
    }

    dispose(): void {
        if (this.mcpProcess) {
            this.mcpProcess.kill();
        }
        this.statusBarItem.dispose();
    }
}

export function activate(context: vscode.ExtensionContext) {
    const extension = new HomeLabMCPExtension(context);
    extension.activate();

    context.subscriptions.push({
        dispose: () => extension.dispose()
    });
}

export function deactivate() {}
