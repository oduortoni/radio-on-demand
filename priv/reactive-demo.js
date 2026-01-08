// Van.js reactive button example
const { button, div } = van.tags;

// Reactive state
const count = van.state(0);
const isActive = van.state(false);

// Reactive button component
const ReactiveButton = () => {
    alert("Hello from ReactiveButton!");
    return div(
        { class: "reactive-demo" },
        button(
            {
                class: () => `demo-btn ${isActive.val ? 'active' : ''}`,
                onclick: () => {
                    count.val++;
                    isActive.val = !isActive.val;
                }
            },
            () => `Clicked ${count.val} times`
        ),
        div(
            { class: "status-text" },
            () => `Status: ${isActive.val ? 'Active' : 'Inactive'}`
        )
    );
};

// Add to page when DOM is ready
van.add(document.getElementById('reactive-demo'), ReactiveButton());